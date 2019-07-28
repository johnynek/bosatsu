package org.bykn.bosatsu

import org.bykn.bosatsu.rankn.NTypeGen
import cats.{Defer, Monad}
import cats.data.{NonEmptyList, StateT}
import org.scalacheck.{Arbitrary, Gen, Shrink}
import cats.implicits._

object Generators {
  val lower: Gen[Char] = Gen.oneOf('a' to 'z')
  val upper: Gen[Char] = Gen.oneOf('A' to 'Z')
  val num: Gen[Char] = Gen.oneOf('0' to '9')
  val identC: Gen[Char] = Gen.frequency((10, lower), (1, upper), (1, num))

  private val emptyRegion: Region = Region(0, 0)

  def nonEmpty[T](t: Gen[T]): Gen[NonEmptyList[T]] =
    for {
      h <- t
      tail <- Gen.listOf(t)
    } yield NonEmptyList(h, tail)

  def nonEmptyN[T](t: Gen[T], n: Int): Gen[NonEmptyList[T]] =
    for {
      h <- t
      cnt <- Gen.choose(0, n)
      tail <- Gen.listOfN(cnt, t)
    } yield NonEmptyList(h, tail)

  val keyWords = Set(
    "if", "ffi", "match", "struct", "enum", "else", "elif",
    "def", "external", "package", "import", "export", "forall",
    "recur", "recursive")

  val lowerIdent: Gen[String] =
    (for {
      c <- lower
      cnt <- Gen.choose(0, 10)
      rest <- Gen.listOfN(cnt, identC)
    } yield (c :: rest).mkString).filter { s=> !keyWords(s) }

  val upperIdent: Gen[String] =
    for {
      c <- upper
      cnt <- Gen.choose(0, 10)
      rest <- Gen.listOfN(cnt, identC)
    } yield (c :: rest).mkString

  val typeRefVarGen: Gen[TypeRef.TypeVar] =
    lowerIdent.map(TypeRef.TypeVar(_))

  val typeRefLambdaGen: Gen[TypeRef.TypeLambda] =
    for {
      e <- Gen.lzy(typeRefGen)
      cnt <- Gen.choose(1, 3)
      args <- Gen.listOfN(cnt, typeRefVarGen)
      nel = NonEmptyList.fromListUnsafe(args)
    } yield TypeRef.TypeLambda(nel, e)

  val opGen: Gen[Identifier.Operator] = {
    val sing = Gen.oneOf(Operators.singleToks).map(Identifier.Operator(_))
    val multi = for {
      c <- Gen.choose(2, 5)
      multiGen = Gen.oneOf(Operators.multiToks)
      ms <- Gen.listOfN(c, multiGen)
    } yield Identifier.Operator(ms.mkString)

    Gen.frequency((4, sing), (1, multi))
  }

  val bindIdentGen: Gen[Identifier.Bindable] =
    Gen.frequency(
      (10, lowerIdent.map { n => Identifier.Name(n) }),
      (1, opGen),
      (1, Arbitrary.arbitrary[String].map { s =>
        Identifier.Backticked(s)
      }))

  val consIdentGen: Gen[Identifier.Constructor] =
    upperIdent.map { n => Identifier.Constructor(n) }

  val typeNameGen: Gen[TypeName] =
    consIdentGen.map(TypeName(_))

  lazy val typeRefGen: Gen[TypeRef] = {
    import TypeRef._

    val tvar = typeRefVarGen
    val tname = typeNameGen.map(TypeRef.TypeName(_))

    val tApply =
      for {
        e <- Gen.oneOf(tvar, tname)
        cnt <- Gen.choose(1, 10)
        args <- Gen.listOfN(cnt, Gen.lzy(typeRefGen))
        nel = NonEmptyList.fromListUnsafe(args)
      } yield TypeApply(e, nel)

    val tLambda = typeRefLambdaGen

    val tTup = Gen
      .choose(0, 3)
      .flatMap(Gen.listOfN(_, typeRefGen))
      .map(TypeRef.TypeTuple(_))

    Gen.frequency(
      (4, tvar),
      (4, tname),
      (1, Gen.zip(Gen.lzy(typeRefGen), Gen.lzy(typeRefGen)).map { case (a, b) => TypeArrow(a, b) }),
      (1, tLambda),
      (1, tTup),
      (1, tApply))
  }

  implicit val shrinkTypeRef: Shrink[TypeRef] =
    Shrink[TypeRef](new Function1[TypeRef, Stream[TypeRef]] {
      def apply(tr: TypeRef): Stream[TypeRef] = {
        import TypeRef._
        tr match {
          case TypeVar(v) => Stream.empty
          case TypeName(n) => Stream.empty
          case TypeArrow(l, r) => Stream(l, r)
          case TypeApply(of, args) => of #:: args.toList.toStream
          case TypeLambda(params, expr) => expr #:: Stream.empty
          case TypeTuple(ts) =>
            def drop(as: List[TypeRef]): Stream[TypeTuple] =
              as match {
                case Nil => Stream.empty
                case h :: tail =>
                  TypeTuple(tail) #:: (drop(tail).map { case TypeTuple(ts) =>
                    TypeTuple(h :: ts)
                  })
              }
            ts.toStream #::: (drop(ts): Stream[TypeRef])
        }
      }
    })

  def commentGen[T](dec: Gen[T]): Gen[CommentStatement[T]] = {
    def cleanNewLine(s: String): String = s.map { c => if (c == '\n') ' ' else c }
    for {
      cs <- nonEmpty(Arbitrary.arbitrary[String])
      t <- dec
    } yield CommentStatement(cs.map(cleanNewLine _), t)
  }

  val argGen: Gen[(Identifier.Bindable, Option[TypeRef])] =
    for {
      arg <- bindIdentGen
      t <- Gen.option(typeRefGen)
    } yield (arg, t)

  def argToPat(arg: (Identifier.Bindable, Option[TypeRef])): Pattern.Parsed =
    arg match {
      case (bn, None) => Pattern.Var(bn)
      case (bn, Some(t)) => Pattern.Annotation(Pattern.Var(bn), t)
    }

  def defGen[T](dec: Gen[T]): Gen[DefStatement[Pattern.Parsed, T]] =
    for {
      name <- bindIdentGen
      args <- Gen.listOf(argGen)
      retType <- Gen.option(typeRefGen)
      body <- dec
    } yield DefStatement(name, args.map(argToPat), retType, body)

  def genSpliceOrItem[A](spliceGen: Gen[A], itemGen: Gen[A]): Gen[ListLang.SpliceOrItem[A]] =
    Gen.oneOf(spliceGen.map(ListLang.SpliceOrItem.Splice(_)),
      itemGen.map(ListLang.SpliceOrItem.Item(_)))

  def genListLangCons[A](spliceGen: Gen[A], itemGen: Gen[A]): Gen[ListLang.Cons[ListLang.SpliceOrItem, A]] = {
    Gen.choose(0, 5)
      .flatMap(Gen.listOfN(_, genSpliceOrItem(spliceGen, itemGen)))
      .map(ListLang.Cons(_))
  }
  def genListLangDictCons[A](itemGen: Gen[A]): Gen[ListLang.Cons[ListLang.KVPair, A]] = {
    Gen.choose(0, 5)
      .flatMap(Gen.listOfN(_,
        Gen.zip(itemGen, itemGen).map { case (k, v) => ListLang.KVPair(k, v) }))
      .map(ListLang.Cons(_))
  }

  def listGen(dec0: Gen[Declaration]): Gen[Declaration.ListDecl] = {
    lazy val filterFn: Declaration => Boolean = {
      case Declaration.Comment(_) => false
      case Declaration.DefFn(_) => false
      case Declaration.Binding(_) => false
      case Declaration.Parens(p) => filterFn(p)
      case Declaration.IfElse(_, _) => false
      case Declaration.Match(_, _, _) => false
      case Declaration.Lambda(_, body) => filterFn(body)
      case Declaration.Apply(f, args, _) =>
        filterFn(f) && args.forall(filterFn)
      case _ => true
    }
    val dec = dec0.filter(filterFn)

    val cons = genListLangCons(dec, dec)

    // TODO we can't parse if since we get confused about it being a ternary expression
    val pat = genPattern(1, useUnion = true)
    val comp = Gen.zip(genSpliceOrItem(dec, dec), pat, dec, Gen.option(dec))
      .map { case (a, b, c, _) => ListLang.Comprehension(a, b, c, None) }

    Gen.oneOf(cons, comp).map(Declaration.ListDecl(_)(emptyRegion))
  }

  def dictGen(dec0: Gen[Declaration]): Gen[Declaration.DictDecl] = {
    lazy val filterFn: Declaration => Boolean = {
      case Declaration.Comment(_) => false
      case Declaration.DefFn(_) => false
      case Declaration.Binding(_) => false
      case Declaration.Parens(p) => filterFn(p)
      case Declaration.IfElse(_, _) => false
      case Declaration.Match(_, _, _) => false
      case Declaration.Lambda(_, body) => filterFn(body)
      case Declaration.Apply(f, args, _) =>
        filterFn(f) && args.forall(filterFn)
      case _ => true
    }
    val dec = dec0.filter(filterFn)

    val cons = genListLangDictCons(dec)

    // TODO we can't parse if since we get confused about it being a ternary expression
    val pat = genPattern(1, useUnion = true)
    val comp = Gen.zip(dec, dec, pat, dec, Gen.option(dec))
      .map { case (k, v, b, c, _) => ListLang.Comprehension(ListLang.KVPair(k, v), b, c, None) }

    Gen.oneOf(cons, comp).map(Declaration.DictDecl(_)(emptyRegion))
  }

  def applyGen(decl: Gen[Declaration]): Gen[Declaration] = {
    val fnGen =
      for {
        fn <- decl
        vp = fn match {
          case v@Declaration.Var(_) => v
          case nonV => Declaration.Parens(nonV)(emptyRegion)
        }
      } yield vp
    applyGen(fnGen, decl, Gen.oneOf(true, false))
  }

  def isVar(d: Declaration): Boolean =
    d match {
      case Declaration.Var(_) => true
      case _ => false
    }

  def applyGen(fnGen: Gen[Declaration], arg: Gen[Declaration], dotApplyGen: Gen[Boolean]): Gen[Declaration] = {
    import Declaration._
    Gen.lzy(for {
      fn <- fnGen
      dotApply <- dotApplyGen
      useDot = dotApply && isVar(fn) // f.bar needs the fn to be a var
      argsGen = if (useDot) arg.map(NonEmptyList(_, Nil)) else nonEmpty(arg)
      args <- argsGen
    } yield Apply(fn, args, ApplyKind.Parens)(emptyRegion)) // TODO this should pass if we use `foo.bar(a, b)` syntax
  }

  def applyOpGen(arg: Gen[Declaration]): Gen[Declaration] =
    Gen.zip(arg, opGen, arg).map { case (l, op, r) =>
      // a few types of things should be in raw ApplyOp, since
      // \x -> x + y is parsed as \x -> (x + y), for instance
      // also, parsing knows about precedence, but randomly
      // making expressions doesn't, so we have to wrap ApplyOp
      import Declaration._
      def protect(d: Declaration): Declaration =
        d match {
          case Var(_) | Apply(_, _, _) | Parens(_) => d
          case notSafe => Parens(d)(emptyRegion)
        }
      ApplyOp(protect(l), op, protect(r))
    }

  def bindGen[A, T](patGen: Gen[A], dec: Gen[Declaration], tgen: Gen[T]): Gen[BindingStatement[A, T]] =
    for {
      b <- patGen
      value0 <- dec
      value = value0 match { case Declaration.Binding(_) => Declaration.Parens(value0)(emptyRegion); case _ => value0 }
      in <- tgen
    } yield BindingStatement(b, value, in)

  def padding[T](tgen: Gen[T], min: Int = 0): Gen[Padding[T]] =
    Gen.zip(Gen.choose(min, 10), tgen)
      .map { case (e, t) => Padding(e, t) }

  def indented[T](tgen: Gen[T]): Gen[Indented[T]] =
    for {
      cnt <- Gen.choose(1, 16)
      t <- tgen
    } yield Indented(cnt, t)

  def lambdaGen(bodyGen: Gen[Declaration]): Gen[Declaration.Lambda] =
    for {
      args <- nonEmpty(bindIdentGen)
      body <- bodyGen
   } yield Declaration.Lambda(args.map(Pattern.Var(_)), body)(emptyRegion)

  def optIndent[A](genA: Gen[A]): Gen[OptIndent[A]] = {
    val indentation = Gen.choose(1, 10)
    indentation.flatMap { i =>

        // TODO support parsing if foo: bar
        //Gen.oneOf(
          padding(genA.map(Indented(i, _)), min = 1).map(OptIndent.notSame(_))
          //,
          //bodyGen.map(Left(_): OptIndent[Declaration]))
    }
  }

  def ifElseGen(bodyGen: Gen[Declaration]): Gen[Declaration.IfElse] = {
    import Declaration._

    val padBody = optIndent(bodyGen)
    val genIf: Gen[(Declaration, OptIndent[Declaration])] =
      Gen.zip(bodyGen, padBody)

    Gen.zip(nonEmptyN(genIf, 2), padBody)
      .map { case (ifs, elsec) => IfElse(ifs, elsec)(emptyRegion) }
  }

  val genStructKind: Gen[Pattern.StructKind] =
    Gen.oneOf(
      Gen.const(Pattern.StructKind.Tuple),
      consIdentGen.map(Pattern.StructKind.Named(_)))

  def genPattern(depth: Int, useUnion: Boolean = true): Gen[Pattern.Parsed] =
    genPatternGen(
      genStructKind,
      typeRefGen,
      depth,
      useUnion,
      useAnnotation = false)

  def genPatternGen[N, T](genName: Gen[N], genT: Gen[T], depth: Int, useUnion: Boolean, useAnnotation: Boolean): Gen[Pattern[N, T]] = {
    val recurse = Gen.lzy(genPatternGen(genName, genT, depth - 1, useUnion, useAnnotation))
    val genVar = bindIdentGen.map(Pattern.Var(_))
    val genWild = Gen.const(Pattern.WildCard)
    val genLitPat = genLit.map(Pattern.Literal(_))

    if (depth <= 0) Gen.oneOf(genVar, genWild, genLitPat)
    else {
      val genNamed = Gen.zip(bindIdentGen, recurse).map { case (n, p) => Pattern.Named(n, p) }
      val genTyped = Gen.zip(recurse, genT)
        .map { case (p, t) => Pattern.Annotation(p, t) }

      val genStruct =  for {
        nm <- genName
        cnt <- Gen.choose(0, 6)
        args <- Gen.listOfN(cnt, recurse)
      } yield Pattern.PositionalStruct(nm, args)

      def makeOneSplice(ps: List[Pattern.ListPart[Pattern[N, T]]]) = {
        val sz = ps.size
        if (sz == 0) Gen.const(ps)
        else Gen.choose(0, sz - 1).flatMap { idx =>
          val splice = Gen.oneOf(
            Gen.const(Pattern.ListPart.WildList),
            bindIdentGen.map { v => Pattern.ListPart.NamedList(v) })

          splice.map { v => ps.updated(idx, v) }
        }
      }

      val genListItem: Gen[Pattern.ListPart[Pattern[N, T]]] =
        recurse.map(Pattern.ListPart.Item(_))

      val genList = Gen.choose(0, 5)
        .flatMap(Gen.listOfN(_, genListItem))
        .flatMap { ls =>
          Gen.oneOf(true, false)
            .flatMap {
              case true => Gen.const(ls)
              case false => makeOneSplice(ls)
            }
        }
        .map(Pattern.ListPat(_))

      val genUnion = Gen.choose(0, 2)
        .flatMap { sz => Gen.zip(recurse, recurse, Gen.listOfN(sz, recurse)) }
        .map {
          case (h0, h1, tail) =>
            Pattern.Union(h0, NonEmptyList(h1, tail))
        }

      val tailGens =
        List(genVar, genWild, genNamed, genLitPat, genStruct, genList)

      val withU = if (useUnion) genUnion :: tailGens else tailGens
      val withT = (if (useAnnotation) genTyped :: withU else withU).toArray
      val len = withT.size
      Gen.choose(0, len - 1).flatMap(withT(_))
    }
  }

  def genCompiledPattern(depth: Int): Gen[Pattern[(PackageName, Identifier.Constructor), rankn.Type]] =
    genPatternGen(Gen.zip(packageNameGen, consIdentGen), NTypeGen.genDepth03, depth, useUnion = true, useAnnotation = true)

  def matchGen(bodyGen: Gen[Declaration]): Gen[Declaration.Match] = {
    import Declaration._

    val padBody = optIndent(bodyGen)


    val genCase: Gen[(Pattern.Parsed, OptIndent[Declaration])] =
      Gen.zip(genPattern(3), padBody)

    for {
      cnt <- Gen.choose(1, 2)
      kind <- Gen.frequency((10, Gen.const(RecursionKind.NonRecursive)), (1, Gen.const(RecursionKind.Recursive)))
      expr <- bodyGen
      cases <- optIndent(nonEmptyN(genCase, cnt))
    } yield Match(kind, expr, cases)(emptyRegion)
  }

  val genLit: Gen[Lit] = {
    val str = for {
      q <- Gen.oneOf('\'', '"')
      //str <- Arbitrary.arbitrary[String]
      str <- lowerIdent // TODO
    } yield Lit.Str(str)

    val bi = Arbitrary.arbitrary[BigInt].map { bi => Lit.Integer(bi.bigInteger) }
    Gen.oneOf(str, bi)
  }

  val identifierGen: Gen[Identifier] =
    Gen.oneOf(bindIdentGen, consIdentGen)

  val varGen: Gen[Declaration.Var] =
    bindIdentGen.map(Declaration.Var(_)(emptyRegion))

  val consDeclGen: Gen[Declaration.Var] =
    consIdentGen.map(Declaration.Var(_)(emptyRegion))

  val unnestedDeclGen: Gen[Declaration] =
    Gen.frequency(
      (1, consDeclGen),
      (2, varGen),
      (1, genLit.map(Declaration.Literal(_)(emptyRegion))))
  /**
   * Generate a Declaration that can be parsed as a pattern
   */
  def patternDecl(depth: Int): Gen[Declaration] = {
    import Declaration._
    val recur = Gen.lzy(patternDecl(depth - 1))

    val applyCons = applyGen(consDeclGen, recur, Gen.const(false))

    if (depth <= 0) unnestedDeclGen
    else Gen.frequency(
      (12, unnestedDeclGen),
      (2, applyCons),
      (1, recur.map(Parens(_)(emptyRegion))),
      (1, genListLangCons(varGen, recur).map(ListDecl(_)(emptyRegion))))
  }

  def simpleDecl(depth: Int): Gen[Declaration] = {
    import Declaration._

    val unnested = unnestedDeclGen

    val recur = Gen.lzy(simpleDecl(depth - 1))
    if (depth <= 0) unnested
    else Gen.frequency(
      (13, unnested),
      (2, lambdaGen(recur)),
      (2, applyGen(recur)),
      (1, applyOpGen(recur)),
      (1, listGen(recur)),
      (1, dictGen(recur)),
      (1, Gen.choose(0, 4).flatMap(Gen.listOfN(_, recur)).map(TupleCons(_)(emptyRegion)))
    )
  }

  def genDeclaration(depth: Int): Gen[Declaration] = {
    import Declaration._

    val unnested = unnestedDeclGen

    val pat: Gen[Pattern.Parsed] = bindIdentGen.map(Pattern.Var(_))
    //val pat = genPattern(0)

    val recur = Gen.lzy(genDeclaration(depth - 1))
    if (depth <= 0) unnested
    else Gen.frequency(
      (13, unnested),
      (2, commentGen(padding(recur, 1)).map(Comment(_)(emptyRegion))), // make sure we have 1 space to prevent comments following each other
      (2, defGen(Gen.zip(optIndent(recur), padding(recur, 1))).map(DefFn(_)(emptyRegion))),
      (2, lambdaGen(recur)),
      (2, applyGen(recur)),
      (1, applyOpGen(simpleDecl(depth - 1))),
      (2, bindGen(pat, recur, padding(recur, 1)).map(Binding(_)(emptyRegion))),
      (1, ifElseGen(recur)),
      (1, listGen(recur)),
      (1, dictGen(recur)),
      (1, matchGen(recur)),
      (1, Gen.choose(0, 4).flatMap(Gen.listOfN(_, recur)).map(TupleCons(_)(emptyRegion)))
    )
  }

  implicit val shrinkDecl: Shrink[Declaration] =
    Shrink[Declaration](new Function1[Declaration, Stream[Declaration]] {
      import Declaration._

      def apply(d: Declaration): Stream[Declaration] =
        d match {
          case Apply(fn, args, _) =>
            val next = fn #:: args.toList.toStream
            next.flatMap(apply _)
          case ao@ApplyOp(left, op, right) =>
            left #:: ao.opVar #:: right #:: Stream.empty
          case Binding(b) =>
            val next = b.value #:: b.in.padded #:: Stream.empty
            next #::: next.flatMap(apply _)
          case DefFn(d) =>
            val (b, r) = d.result
            val inner = b.get #:: r.padded #:: Stream.empty
            inner #::: inner.flatMap(apply _)
          case IfElse(ifCases, elseCase) =>
            elseCase.get #:: ifCases.toList.toStream.map(_._2.get)
          case Match(_, typeName, args) =>
            args.get.toList.toStream.flatMap {
              case (_, decl) => decl.get #:: apply(decl.get)
            }
          // the rest can't be shrunk
          case Comment(c) => Stream.empty
          case Lambda(args, body) => body #:: Stream.empty
          case Literal(_) => Stream.empty
          case Parens(p) => p #:: Stream.empty
          case TupleCons(Nil) => Stream.empty
          case TupleCons(h :: tail) => h #:: TupleCons(tail)(emptyRegion) #:: apply(TupleCons(tail)(emptyRegion))
          case Var(_) => Stream.empty
          case ListDecl(ListLang.Cons(items)) =>
            items.map(_.value).toStream
          case ListDecl(ListLang.Comprehension(a, _, c, d)) =>
            (a.value :: c :: d.toList).toStream
          case DictDecl(ListLang.Cons(items)) =>
            items.toStream.flatMap { kv => Stream(kv.key, kv.value) }
          case DictDecl(ListLang.Comprehension(a, _, c, d)) =>
            (a.key :: a.value :: c :: d.toList).toStream
        }
    })

  implicit val shrinkStmt: Shrink[Statement] =
    Shrink[Statement](new Function1[Statement, Stream[Statement]] {
      import Statement._

      def apply(s: Statement): Stream[Statement] = s match {
        case EndOfFile => Stream.empty
        case Bind(bs@BindingStatement(_, d, _)) =>
          shrinkDecl.shrink(d).flatMap { sd =>
            Bind(bs.copy(value = sd)).toStream
          }
        case Def(ds) =>
          ds.result match {
            case (body, Padding(l, in)) =>
              body.traverse(shrinkDecl.shrink(_))
                .flatMap { bod =>
                  apply(in).map { rest => Def(ds.copy(result = (bod, Padding(1, rest)))) }
                }
          }
        case rest => rest.toStream.tail
      }
    })

  val constructorGen: Gen[(Identifier.Constructor, List[(Identifier.Bindable, Option[TypeRef])])] =
    for {
      name <- consIdentGen
      argc <- Gen.choose(0, 5)
      args <- Gen.listOfN(argc, argGen)
    } yield (name, args)


  def genStruct(tail: Gen[Statement]): Gen[Statement] =
    Gen.zip(constructorGen, Gen.listOf(typeRefVarGen), padding(tail, 1))
      .map { case ((name, args), ta, rest) =>
        Statement.Struct(name, NonEmptyList.fromList(ta.distinct), args, rest)
      }

  def genExternalStruct(tail: Gen[Statement]): Gen[Statement] =
    for {
      name <- consIdentGen
      argc <- Gen.choose(0, 5)
      args <- Gen.listOfN(argc, typeRefVarGen)
      rest <- padding(tail, 1)
    } yield Statement.ExternalStruct(name, args, rest)

  def genExternalDef(tail: Gen[Statement]): Gen[Statement] =
    for {
      name <- bindIdentGen
      argc <- Gen.choose(0, 5)
      argG = Gen.zip(bindIdentGen, typeRefGen)
      args <- Gen.listOfN(argc, argG)
      res <- typeRefGen
      rest <- padding(tail, 1)
    } yield Statement.ExternalDef(name, args, res, rest)

  def genEnum(tail: Gen[Statement]): Gen[Statement] =
    for {
      name <- consIdentGen
      ta <- Gen.listOf(typeRefVarGen)
      consc <- Gen.choose(1, 5)
      i <- Gen.choose(1, 16)
      cons <- optIndent(nonEmptyN(constructorGen, consc))
      rest <- padding(tail, 1)
    } yield Statement.Enum(name, NonEmptyList.fromList(ta.distinct), cons, rest)

  def genStatement(depth: Int): Gen[Statement] = {
    val recur = Gen.lzy(genStatement(depth-1))
    val decl = genDeclaration(depth)
    // TODO make more powerful
    val pat: Gen[Pattern.Parsed] = genPattern(1)
    Gen.frequency(
      (1, bindGen(pat, decl, padding(recur, 1)).map(Statement.Bind(_))),
      (1, commentGen(padding(recur, 1)
        .map {
          case Padding(0, c@Statement.Comment(_)) => Padding[Statement](1, c) // make sure not two back to back comments
          case other => other
        }).map(Statement.Comment(_))),
      (1, defGen(Gen.zip(optIndent(decl), padding(recur, 1))).map(Statement.Def(_))),
      (1, genStruct(recur)),
      (1, genExternalStruct(recur)),
      (1, genExternalDef(recur)),
      (1, genEnum(recur)),
      (3, Gen.const(Statement.EndOfFile)))
  }

  val packageNameGen: Gen[PackageName] =
    for {
      pc <- Gen.choose(1, 5)
      (h :: tail) <- Gen.listOfN(pc, upperIdent)
    } yield PackageName(NonEmptyList(h, tail))

  val importedNameGen: Gen[ImportedName[Unit]] = {
    def rename(g: Gen[Identifier]): Gen[ImportedName[Unit]] =
      Gen.zip(g, g).map { case (f, t) => ImportedName.Renamed(f, t, ()) }

    def orig(g: Gen[Identifier]): Gen[ImportedName[Unit]] =
      g.map(ImportedName.OriginalName(_, ()))

    def in(g: Gen[Identifier]) = Gen.oneOf(rename(g), orig(g))

    Gen.oneOf(in(bindIdentGen), in(consIdentGen))
  }

  val importGen: Gen[Import[PackageName, Unit]] =
    for {
      p <- packageNameGen
      importCount <- Gen.choose(1, 10)
      (h :: tail) <- Gen.listOfN(importCount, importedNameGen)
    } yield Import(p, NonEmptyList(h, tail))

  val exportedNameGen: Gen[ExportedName[Unit]] =
    Gen.oneOf(
      bindIdentGen.map(ExportedName.Binding(_, ())),
      consIdentGen.map(ExportedName.TypeName(_, ())),
      consIdentGen.map(ExportedName.Constructor(_, ())))

  def smallList[A](g: Gen[A]): Gen[List[A]] =
    Gen.choose(0, 8).flatMap(Gen.listOfN(_, g))

  def packageGen(depth: Int): Gen[Package.Parsed] =
    for {
      p <- packageNameGen
      ec <- Gen.choose(0, 10)
      imports <- smallList(importGen)
      exports <- Gen.listOfN(ec, exportedNameGen)
      body <- genStatement(depth)
    } yield Package(p, imports, exports, body)


  def genDefinedType[A](p: PackageName, inner: Gen[A], genType: Gen[rankn.Type]): Gen[rankn.DefinedType[A]] =
    for {
      t <- typeNameGen
      params0 <- smallList(Gen.zip(NTypeGen.genBound, inner))
      params = params0.toMap.toList // don't generate duplicate type parameters
      genCons: Gen[(Identifier.Constructor, List[(Identifier.Bindable, rankn.Type)], rankn.Type)] =
        for {
          cons <- consIdentGen
          ps <- smallList(Gen.zip(bindIdentGen, genType))
          res = rankn.DefinedType.constructorValueType(p, t, params.map(_._1), ps.map(_._2))
        } yield (cons, ps, res)
      cons0 <- smallList(genCons)
      cons = cons0.map { case trip@(c, _, _) => (c, trip) }.toMap.values.toList
    } yield rankn.DefinedType(p, t, params, cons)

  def typeEnvGen[A](p: PackageName, inner: Gen[A]): Gen[rankn.TypeEnv[A]] =
    smallList(genDefinedType(p, inner, NTypeGen.genDepth03))
      .map(rankn.TypeEnv.fromDefinitions(_))

  def exportGen[A](te: rankn.TypeEnv[A]): Gen[ExportedName[Referant[A]]] = {
    def bind(genTpe: Gen[rankn.Type]) = for {
      n <- bindIdentGen
      t <- genTpe
    } yield ExportedName.Binding(n, Referant.Value(t))

    te.allDefinedTypes match {
      case Nil => bind(NTypeGen.genDepth03)
      case dts0 =>
        // only make one of each type
        val dts = dts0.map { dt => (dt.name.ident, dt) }.toMap.values.toList

        val b = bind(Gen.oneOf(NTypeGen.genDepth03, Gen.oneOf(dts).map(_.toTypeTyConst)))
        val genExpT = Gen.oneOf(dts)
          .map { dt =>
            ExportedName.TypeName(dt.name.ident, Referant.DefinedT(dt))
          }

        dts.filter(_.constructors.nonEmpty) match {
          case Nil => Gen.oneOf(b, genExpT)
          case nonEmpty =>
            val c = for {
              dt <- Gen.oneOf(nonEmpty)
              (c, ps, tpe) <- Gen.oneOf(dt.constructors)
            } yield ExportedName.Constructor(c, Referant.Constructor(c, dt, ps, tpe))
            Gen.oneOf(b, genExpT, c)
        }
    }
  }

  val interfaceGen: Gen[Package.Interface] =
    for {
      p <- packageNameGen
      te <- typeEnvGen(p, Gen.oneOf(Variance.co, Variance.phantom, Variance.contra, Variance.in))
      exs0 <- smallList(exportGen(te))
      exs = exs0.map { ex => (ex.name, ex) }.toMap.values.toList // don't duplicate exported names
    } yield Package(p, Nil, exs, ())

  /**
   * This is a totally random, and not well typed expression.
   * It is suitable for some tests, but it is not a valid output
   * of a typechecking process
   */
  def genTypedExpr[A](genTag: Gen[A], depth: Int, typeGen: Gen[rankn.Type]): Gen[TypedExpr[A]] = {
    val recurse = Gen.lzy(genTypedExpr(genTag, depth - 1, typeGen))
    val lit = Gen.zip(genLit, NTypeGen.genDepth03, genTag).map { case (l, tpe, tag) => TypedExpr.Literal(l, tpe, tag) }
    // only literal doesn't recurse
    if (depth <= 0) lit
    else {
      val genGeneric =
        Gen.zip(Generators.nonEmpty(NTypeGen.genBound), recurse, genTag)
          .map { case (vs, t, tag) => TypedExpr.Generic(vs, t, tag) }

      val ann =
        Gen.zip(recurse, typeGen, genTag)
          .map { case (te, tpe, tag) => TypedExpr.Annotation(te, tpe, tag) }

      val lam =
        Gen.zip(bindIdentGen, typeGen, recurse, genTag)
          .map { case (n, tpe, res, tag) => TypedExpr.AnnotatedLambda(n, tpe, res, tag) }

      val varGen =
        Gen.zip(Gen.option(packageNameGen), identifierGen, typeGen, genTag)
          .map { case (op, n, t, tag) => TypedExpr.Var(op, n, t, tag) }

      val app =
        Gen.zip(recurse, recurse, typeGen, genTag)
          .map { case (fn, arg, tpe, tag) => TypedExpr.App(fn, arg, tpe, tag) }

      val let =
        Gen.zip(bindIdentGen, recurse, recurse, Gen.oneOf(RecursionKind.NonRecursive, RecursionKind.Recursive), genTag)
          .map { case (n, ex, in, rec, tag) => TypedExpr.Let(n, ex, in, rec, tag) }

      val matchGen =
        Gen.zip(recurse, Gen.choose(1, 4).flatMap(nonEmptyN(Gen.zip(genCompiledPattern(depth), recurse), _)), genTag)
          .map { case (arg, branches, tag) => TypedExpr.Match(arg, branches, tag) }

      Gen.oneOf(genGeneric, ann, lam, varGen, app, let, lit, matchGen)
    }
  }

  private implicit val genMonad: Monad[Gen] with Defer[Gen] =
    new Monad[Gen] with Defer[Gen] {
      def pure[A](a: A): Gen[A] = Gen.const(a)
      def defer[A](ga: => Gen[A]): Gen[A] = Gen.lzy(ga)
      override def product[A, B](ga: Gen[A], gb: Gen[B]) = Gen.zip(ga, gb)
      def flatMap[A, B](ga: Gen[A])(fn: A => Gen[B]) = ga.flatMap(fn)
      override def map[A, B](ga: Gen[A])(fn: A => B): Gen[B] = ga.map(fn)
      def tailRecM[A, B](a: A)(fn: A => Gen[Either[A, B]]): Gen[B] =
        fn(a).flatMap {
          case Left(a) =>
            // TODO in the latest scalacheck, there is native tailRecM
            // but we can't implement a safe one without using private details
            tailRecM(a)(fn)
          case Right(b) =>
            Gen.const(b)
        }
    }

  def shuffle[A](as: List[A]): Gen[List[A]] =
    as match {
      case (Nil | (_ :: Nil)) => Gen.const(as)
      case a :: b :: Nil =>
        Gen.oneOf(as, b :: a :: Nil)
      case _ =>
        val size = as.size
        def loop(idx: Int): Gen[List[Int]] =
          if (idx >= size) Gen.const(Nil)
          else
            Gen.zip(Gen.choose(idx, size - 1), loop(idx + 1))
              .map { case (h, tail) => h :: tail }

        loop(0).map { swaps =>
          val ary = as.toBuffer
          swaps.zipWithIndex.foreach { case (targ, src) =>
            val temp = ary(targ)
            ary(targ) = ary(src)
            ary(src) = temp
          }
          ary.toList
        }
      }

  def genOnePackage[A](genA: Gen[A], existing: Map[PackageName, Package.Typed[A]]): Gen[Package.Typed[A]] = {
    val genDeps: Gen[Map[PackageName, Package.Typed[A]]] =
      Gen.frequency(
        (5, Gen.const(Map.empty)), // usually have no deps, otherwise the graph gets enormous
        (1, shuffle(existing.toList).map(_.take(2).toMap))
      )

    def impFromExp(exp: List[(Package.Interface, ExportedName[Referant[Variance]])]): Gen[List[Import[Package.Interface, NonEmptyList[Referant[Variance]]]]] =
      exp.groupBy(_._1)
        .toList
        .traverse { case (p, exps) =>
          val genImps: Gen[List[ImportedName[NonEmptyList[Referant[Variance]]]]] =
            exps.groupBy(_._2.name)
              .iterator
              .toList
              .traverse { case (ident, exps) =>
                // we know exps is non-empty because it is from a groupBy.
                // note they all share an identifier also
                val neExps = NonEmptyList.fromListUnsafe(exps.map(_._2.tag))
                // the only thing to decide here
                // is if we alias or not
                val genImpName: Gen[Option[Identifier]] =
                  ident match {
                    case Identifier.Constructor(_) =>
                      Gen.option(consIdentGen)
                    case _ =>
                      Gen.option(bindIdentGen)
                  }

                genImpName.map {
                  case None =>
                    ImportedName.OriginalName(ident, neExps)
                  case Some(newName) =>
                    ImportedName.Renamed(ident, newName, neExps)
                }
              }

          genImps.map { is =>
            Import(p, NonEmptyList.fromListUnsafe(is))
          }
        }

    val genImports: Gen[List[Import[Package.Interface, NonEmptyList[Referant[Variance]]]]] =
      genDeps.flatMap { packs =>
        val exps: List[(Package.Interface, ExportedName[Referant[Variance]])] =
          (for {
            (_, p) <- packs.iterator
            exp <- p.exports
          } yield (Package.interfaceOf(p), exp)).toList

        for {
          sexps <- shuffle(exps)
          cnt <- Gen.choose(0, 5) // TODO, maybe import more...
          imp <- impFromExp(sexps.take(cnt))
        } yield imp
      }

    def definedTypesFromImp(i: Import[Package.Interface, NonEmptyList[Referant[Variance]]]): List[rankn.Type.Const] =
      i.items.toList.flatMap { in =>
        in.tag.toList.flatMap {
          case Referant.DefinedT(dt) => dt.toTypeConst :: Nil
          case Referant.Constructor(_, dt, _, _) => dt.toTypeConst :: Nil
          case Referant.Value(_) => Nil
        }
      }

    def genTypeEnv(pn: PackageName,
      imps: List[Import[Package.Interface, NonEmptyList[Referant[Variance]]]]): StateT[Gen, (rankn.TypeEnv[Variance], Set[Identifier.Bindable]), Unit] =
        StateT.get[Gen, (rankn.TypeEnv[Variance], Set[Identifier.Bindable])]
          .flatMap { case (te, extDefs) =>
            StateT.liftF(Gen.choose(0, 9))
              .flatMap {
                case 0 =>
                  // 1 in 10 chance of stopping
                  StateT.pure[Gen, (rankn.TypeEnv[Variance], Set[Identifier.Bindable]), Unit](())
                case _ =>
                  // add something:
                  val tyconsts =
                    te.allDefinedTypes.map(_.toTypeConst) ++
                      imps.flatMap(definedTypesFromImp)
                  val theseTypes = NTypeGen.genDepth(4, if (tyconsts.isEmpty) None else Some(Gen.oneOf(tyconsts)))
                  val genV: Gen[Variance] =
                    Gen.oneOf(Variance.co, Variance.contra, Variance.in, Variance.phantom)
                  val genDT = genDefinedType(pn, genV, theseTypes)
                  val genEx: Gen[(Identifier.Bindable, rankn.Type)] =
                    Gen.zip(bindIdentGen, theseTypes)

                  // we can do one of the following:
                  // 1: add an external value
                  // 2: add a defined type
                  StateT.liftF(Gen.frequency(
                    (5, genDT.map { dt => (te.addDefinedTypeAndConstructors(dt), extDefs) }),
                    (1, genEx.map { case (b, t) => (te.addExternalValue(pn, b, t), extDefs + b) })))
                    .flatMap(StateT.set(_))
              }
          }

    def genLets(te: rankn.TypeEnv[Variance],
      exts: Set[Identifier.Bindable]): Gen[List[(Identifier.Bindable, RecursionKind, TypedExpr[A])]] = {
        val allTC = te.allDefinedTypes.map(_.toTypeConst)
        val theseTypes = NTypeGen.genDepth(4, if (allTC.isEmpty) None else Some(Gen.oneOf(allTC)))
        val oneLet = Gen.zip(bindIdentGen,
          Gen.oneOf(RecursionKind.NonRecursive, RecursionKind.Recursive),
          genTypedExpr(genA, 4, theseTypes))

        Gen.choose(0, 6).flatMap(Gen.listOfN(_, oneLet))
      }

    def genProg(
      pn: PackageName,
      imps: List[Import[Package.Interface, NonEmptyList[Referant[Variance]]]]): Gen[Program[rankn.TypeEnv[Variance], TypedExpr[A], Any]] =
        genTypeEnv(pn, imps)
          .runS((rankn.TypeEnv.empty, Set.empty))
          .flatMap { case (te, b) =>
            genLets(te, b).map(Program(te, _, b.toList.sorted, ()))
          }

    /*
     * Exports are types, constructors, or values
     */
    def genExports(pn: PackageName, p: Program[rankn.TypeEnv[Variance], TypedExpr[A], Any]): Gen[List[ExportedName[Referant[Variance]]]] = {
      def expnames: List[ExportedName[Referant[Variance]]] =
        p.lets.map { case (n, _, te) =>
          ExportedName.Binding(n, Referant.Value(te.getType))
        }
      def exts: List[ExportedName[Referant[Variance]]] =
        p.externalDefs.flatMap { n =>
          p.types.getValue(pn, n).map { t => ExportedName.Binding(n, Referant.Value(t)) }
        }

      def cons: List[ExportedName[Referant[Variance]]] =
        p.types.allDefinedTypes.flatMap { dt =>
          if (dt.packageName == pn) {
            val dtex = ExportedName.TypeName(dt.name.ident, Referant.DefinedT(dt))
            val cons = dt.constructors.map { case (c, p, t) =>
              ExportedName.Constructor(c, Referant.Constructor(c, dt, p, t))
            }

            dtex :: cons
          }
          else Nil
        }

      for {
        cnt <- Gen.choose(0, 5)
        lst <- shuffle(expnames ::: exts ::: cons)
      } yield lst.take(cnt)
    }

    for {
      pn <- packageNameGen
      imps <- genImports
      prog <- genProg(pn, imps)
      exps <- genExports(pn, prog)
    } yield Package(pn, imps, exps, prog)
  }

  def genPackagesSt[A](genA: Gen[A], maxSize: Int): StateT[Gen, Map[PackageName, Package.Typed[A]], Unit] =
    StateT.get[Gen, Map[PackageName, Package.Typed[A]]]
      .flatMap { m =>
        if (m.size >= maxSize) StateT.pure(())
        else {
          // make one more and try again
          for {
            p <- StateT.liftF[Gen, Map[PackageName, Package.Typed[A]], Package.Typed[A]](genOnePackage(genA, m))
            _ <- StateT.set[Gen, Map[PackageName, Package.Typed[A]]](m.updated(p.name, p))
            _ <- genPackagesSt(genA, maxSize)
          } yield ()
        }
      }

  def genPackage[A](genA: Gen[A], maxSize: Int): Gen[Map[PackageName, Package.Typed[A]]] =
    genPackagesSt(genA, maxSize).runS(Map.empty)
}
