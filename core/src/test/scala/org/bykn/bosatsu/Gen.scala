package org.bykn.bosatsu

import cats.data.NonEmptyList
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

  val typeRefLambdaGen: Gen[TypeRef.TypeLambda] =
    for {
      e <- Gen.lzy(typeRefGen)
      cnt <- Gen.choose(1, 3)
      tvar = lowerIdent.map(TypeRef.TypeVar(_))
      args <- Gen.listOfN(cnt, tvar)
      nel = NonEmptyList.fromListUnsafe(args)
    } yield TypeRef.TypeLambda(nel, e)

  lazy val typeRefGen: Gen[TypeRef] = {
    import TypeRef._

    val tvar = lowerIdent.map(TypeVar(_))
    val tname = upperIdent.map(TypeName(_))

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

  def commentGen[T](dec: Gen[T]): Gen[CommentStatement[T]] = {
    def cleanNewLine(s: String): String = s.map { c => if (c == '\n') ' ' else c }
    for {
      cs <- nonEmpty(Arbitrary.arbitrary[String])
      t <- dec
    } yield CommentStatement(cs.map(cleanNewLine _), t)
  }

  val argGen: Gen[(String, Option[TypeRef])] =
    for {
      arg <- lowerIdent
      t <- Gen.option(typeRefGen)
    } yield (arg, t)

  def defGen[T](dec: Gen[T]): Gen[DefStatement[T]] =
    for {
      name <- lowerIdent
      args <- Gen.listOf(argGen)
      retType <- Gen.option(typeRefGen)
      body <- dec
    } yield DefStatement(name, args, retType, body)

  def genSpliceOrItem[A](spliceGen: Gen[A], itemGen: Gen[A]): Gen[ListLang.SpliceOrItem[A]] =
    Gen.oneOf(spliceGen.map(ListLang.SpliceOrItem.Splice(_)),
      itemGen.map(ListLang.SpliceOrItem.Item(_)))

  def genListLangCons[A](spliceGen: Gen[A], itemGen: Gen[A]): Gen[ListLang.Cons[A]] = {
    Gen.choose(0, 10)
      .flatMap(Gen.listOfN(_, genSpliceOrItem(spliceGen, itemGen))
      .map(ListLang.Cons(_)))
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

  def applyGen(decl: Gen[Declaration]): Gen[Declaration] = {
    val fnGen =
      for {
        fn <- decl
        vp = fn match {
          case v@Declaration.Var(_) => v
          case c@Declaration.Constructor(_) => c
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
    } yield Apply(fn, args, false)(emptyRegion)) // TODO this should pass if we use `foo.bar(a, b)` syntax
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
      args <- nonEmpty(lowerIdent)
      body <- bodyGen
   } yield Declaration.Lambda(args, body)(emptyRegion)

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

  def genPattern(depth: Int, useUnion: Boolean = true): Gen[Pattern[Option[String], TypeRef]] = {
    val recurse = Gen.lzy(genPattern(depth - 1, useUnion))
    val genVar = lowerIdent.map(Pattern.Var(_))
    val genWild = Gen.const(Pattern.WildCard)
    val genLitPat = genLit.map(Pattern.Literal(_))

    if (depth <= 0) Gen.oneOf(genVar, genWild, genLitPat)
    else {
      val genTyped = Gen.zip(recurse, typeRefGen)
        .map { case (p, t) => Pattern.Annotation(p, t) }

      val genStruct =  for {
        nm <- Gen.option(upperIdent)
        cnt <- Gen.choose(0, 6)
        args <- Gen.listOfN(cnt, recurse)
      } yield Pattern.PositionalStruct(nm, args)

      def makeOneSplice(ps: List[Either[Option[String], Pattern[Option[String], TypeRef]]]) = {
        val sz = ps.size
        if (sz == 0) Gen.const(ps)
        else Gen.choose(0, sz - 1).flatMap { idx =>
          val splice = Gen.oneOf(
            Gen.const(Left(None)),
            lowerIdent.map { v => Left(Some(v)) })

          splice.map { v => ps.updated(idx, v) }
        }
      }

      val genListItem: Gen[Either[Option[String], Pattern[Option[String], TypeRef]]] =
        recurse.map(Right(_))

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

      if (useUnion) Gen.oneOf(genVar, genWild, genLitPat, genStruct, genList, genUnion /*, genTyped */)
      else Gen.oneOf(genVar, genWild, genLitPat, genStruct, genList/*, genTyped */)
    }
  }

  def matchGen(bodyGen: Gen[Declaration]): Gen[Declaration.Match] = {
    import Declaration._

    val padBody = optIndent(bodyGen)


    val genCase: Gen[(Pattern[Option[String], TypeRef], OptIndent[Declaration])] =
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

  /**
   * Generate a Declaration that can be parsed as a pattern
   */
  def patternDecl(depth: Int): Gen[Declaration] = {
    import Declaration._
    val recur = Gen.lzy(patternDecl(depth - 1))
    val cons0 = upperIdent.map(Constructor(_)(emptyRegion))
    val varGen = lowerIdent.map(Var(_)(emptyRegion))
    val unnested = Gen.oneOf(
      cons0,
      varGen,
      genLit.map(Literal(_)(emptyRegion)))

    val applyCons = applyGen(cons0, recur, Gen.const(false))

    if (depth <= 0) unnested
    else Gen.frequency(
      (12, unnested),
      (2, applyCons),
      (1, recur.map(Parens(_)(emptyRegion))),
      (1, genListLangCons(varGen, recur).map(ListDecl(_)(emptyRegion))))
  }


  def genDeclaration(depth: Int): Gen[Declaration] = {
    import Declaration._

    val unnested = Gen.oneOf(
      lowerIdent.map(Var(_)(emptyRegion)),
      upperIdent.map(Constructor(_)(emptyRegion)),
      genLit.map(Literal(_)(emptyRegion)))

    val pat: Gen[Pattern[Option[String], TypeRef]] = lowerIdent.map(Pattern.Var(_))
    //val pat = genPattern(0)

    val recur = Gen.lzy(genDeclaration(depth - 1))
    if (depth <= 0) unnested
    else Gen.frequency(
      (12, unnested),
      (2, commentGen(padding(recur, 1)).map(Comment(_)(emptyRegion))), // make sure we have 1 space to prevent comments following each other
      (2, defGen(Gen.zip(optIndent(recur), padding(recur, 1))).map(DefFn(_)(emptyRegion))),
      (2, lambdaGen(recur)),
      (2, applyGen(recur)),
      (2, bindGen(pat, recur, padding(recur, 1)).map(Binding(_)(emptyRegion))),
      (1, ifElseGen(recur)),
      (1, listGen(recur)),
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
          case Constructor(name) => Stream.empty
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

  val constructorGen: Gen[(String, List[(String, Option[TypeRef])])] =
    for {
      name <- upperIdent
      argc <- Gen.choose(0, 5)
      args <- Gen.listOfN(argc, argGen)
    } yield (name, args)


  def genStruct(tail: Gen[Statement]): Gen[Statement] =
    Gen.zip(constructorGen, padding(tail, 1))
      .map { case ((name, args), rest) =>
        Statement.Struct(name, args, rest)
      }

  def genExternalStruct(tail: Gen[Statement]): Gen[Statement] =
    for {
      name <- upperIdent
      argc <- Gen.choose(0, 5)
      args <- Gen.listOfN(argc, lowerIdent)
      rest <- padding(tail, 1)
    } yield Statement.ExternalStruct(name, args.map(TypeRef.TypeVar(_)), rest)

  def genExternalDef(tail: Gen[Statement]): Gen[Statement] =
    for {
      name <- lowerIdent
      argc <- Gen.choose(0, 5)
      argG = Gen.zip(lowerIdent, typeRefGen)
      args <- Gen.listOfN(argc, argG)
      res <- typeRefGen
      rest <- padding(tail, 1)
    } yield Statement.ExternalDef(name, args, res, rest)

  def genEnum(tail: Gen[Statement]): Gen[Statement] =
    for {
      name <- upperIdent
      consc <- Gen.choose(1, 5)
      i <- Gen.choose(1, 16)
      cons <- optIndent(nonEmptyN(constructorGen, consc))
      rest <- padding(tail, 1)
    } yield Statement.Enum(name, cons, rest)

  def genStatement(depth: Int): Gen[Statement] = {
    val recur = Gen.lzy(genStatement(depth-1))
    val decl = genDeclaration(depth)
    // TODO make more powerful
    val pat: Gen[Pattern[Option[String], TypeRef]] = genPattern(1)
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
    def rename(g: Gen[String]): Gen[ImportedName[Unit]] =
      Gen.zip(g, g).map { case (f, t) => ImportedName.Renamed(f, t, ()) }

    def orig(g: Gen[String]): Gen[ImportedName[Unit]] =
      g.map(ImportedName.OriginalName(_, ()))

    def in(g: Gen[String]) = Gen.oneOf(rename(g), orig(g))

    Gen.oneOf(in(lowerIdent), in(upperIdent))
  }

  val importGen: Gen[Import[PackageName, Unit]] =
    for {
      p <- packageNameGen
      importCount <- Gen.choose(1, 10)
      (h :: tail) <- Gen.listOfN(importCount, importedNameGen)
    } yield Import(p, NonEmptyList(h, tail))

  val exportedNameGen: Gen[ExportedName[Unit]] =
    Gen.oneOf(
      lowerIdent.map(ExportedName.Binding(_, ())),
      upperIdent.map(ExportedName.TypeName(_, ())),
      upperIdent.map(ExportedName.Constructor(_, ())))

  def packageGen(depth: Int): Gen[Package.Parsed] =
    for {
      p <- packageNameGen
      ic <- Gen.choose(0, 8)
      ec <- Gen.choose(0, 10)
      imports <- Gen.listOfN(ic, importGen)
      exports <- Gen.listOfN(ec, exportedNameGen)
      body <- genStatement(depth)
    } yield Package(p, imports, exports, body)
}
