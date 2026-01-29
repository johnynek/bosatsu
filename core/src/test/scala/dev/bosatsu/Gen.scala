package dev.bosatsu

import dev.bosatsu.rankn.NTypeGen
import cats.Traverse
import cats.data.{NonEmptyList, StateT}
import org.scalacheck.{Arbitrary, Gen, Shrink}
import cats.implicits._

import rankn.NTypeGen.{consIdentGen, packageNameGen, lowerIdent, typeNameGen}

import Declaration.NonBinding

import MonadGen.genMonad

object Generators {
  val lower: Gen[Char] = Gen.oneOf('a' to 'z')
  val upper: Gen[Char] = Gen.oneOf('A' to 'Z')
  val num: Gen[Char] = Gen.oneOf('0' to '9')
  val identC: Gen[Char] = Gen.frequency((10, lower), (1, upper), (1, num))

  val genCodePoints: Gen[Int] =
    Gen.frequency(
      (10, Gen.choose(0, 0xd7ff)),
      (
        1,
        Gen.choose(0, 0x10ffff).filterNot { cp =>
          (0xd800 <= cp && cp <= 0xdfff)
        }
      )
    )

  val genValidUtf: Gen[String] =
    Gen
      .listOf(genCodePoints)
      .map { points =>
        val bldr = new java.lang.StringBuilder
        points.foreach(bldr.appendCodePoint(_))
        bldr.toString
      }

  val whiteSpace: Gen[String] =
    Gen.listOf(Gen.oneOf(' ', '\t', '\n')).map(_.mkString)

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

  val typeRefVarGen: Gen[TypeRef.TypeVar] =
    lowerIdent.map(TypeRef.TypeVar(_))

  val typeRefLambdaGen: Gen[TypeRef.TypeForAll] =
    for {
      e <- Gen.lzy(typeRefGen)
      cnt <- Gen.choose(1, 3)
      args <- Gen.listOfN(
        cnt,
        Gen.zip(typeRefVarGen, Gen.option(NTypeGen.genKind))
      )
      nel = NonEmptyList.fromListUnsafe(args)
    } yield TypeRef.TypeForAll(nel, e)

  val typeRefExistsGen: Gen[TypeRef.TypeExists] =
    for {
      e <- Gen.lzy(typeRefGen)
      cnt <- Gen.choose(1, 3)
      args <- Gen.listOfN(
        cnt,
        Gen.zip(typeRefVarGen, Gen.option(NTypeGen.genKind))
      )
      nel = NonEmptyList.fromListUnsafe(args)
    } yield TypeRef.TypeExists(nel, e)

  val opGen: Gen[Identifier.Operator] = {
    val sing = Gen.oneOf(Operators.singleToks).map(Identifier.Operator(_))
    lazy val multi: Gen[Identifier.Operator] =
      for {
        c <- Gen.choose(2, 5)
        multiGen = Gen.oneOf(Operators.multiToks)
        ms <- Gen.listOfN(c, multiGen)
        asStr = ms.mkString
        res <-
          (if ((asStr != "<-") && (asStr != "->"))
             Gen.const(Identifier.Operator(asStr))
           else multi)
      } yield res

    Gen.frequency((4, sing), (1, multi))
  }

  val bindIdentGen: Gen[Identifier.Bindable] =
    Gen.frequency(
      (
        10,
        lowerIdent.filter(n => !Declaration.keywords(n)).map { n =>
          Identifier.Name(n)
        }
      ),
      (1, opGen),
      (
        1,
        Arbitrary.arbitrary[String].map { s =>
          Identifier.Backticked(s)
        }
      )
    )

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
      (5, tvar),
      (5, tname),
      (
        1,
        Gen
          .zip(Gen.lzy(smallNonEmptyList(typeRefGen, 4)), Gen.lzy(typeRefGen))
          .map { case (a, b) => TypeArrow(a, b) }
      ),
      (1, tLambda),
      (1, typeRefExistsGen),
      (1, tTup),
      (1, tApply)
    )
  }

  implicit val shrinkTypeRef: Shrink[TypeRef] =
    Shrink.withLazyList[TypeRef] { tr =>
      import TypeRef._
      tr match {
        case TypeVar(_) | TypeName(_) => LazyList.empty
        case TypeArrow(l, r)          =>
          r #:: l.toList.to(LazyList)
        case TypeApply(of, args)   => of #:: args.toList.to(LazyList)
        case TypeForAll(par, expr) =>
          val rest = NonEmptyList.fromList(par.tail) match {
            case None      => LazyList.empty
            case Some(nel) => TypeForAll(nel, expr) #:: LazyList.empty
          }
          expr #:: rest
        case TypeExists(par, expr) =>
          val rest = NonEmptyList.fromList(par.tail) match {
            case None      => LazyList.empty
            case Some(nel) => TypeExists(nel, expr) #:: LazyList.empty
          }
          expr #:: rest
        case TypeTuple(ts) =>
          def drop(as: List[TypeRef]): LazyList[TypeTuple] =
            as match {
              case Nil       => LazyList.empty
              case h :: tail =>
                TypeTuple(tail) #:: (drop(tail).map { case TypeTuple(ts) =>
                  TypeTuple(h :: ts)
                })
            }
          ts.to(LazyList) #::: (drop(ts): LazyList[TypeRef])
      }
    }

  def commentGen[T](dec: Gen[T]): Gen[CommentStatement[T]] = {
    def cleanNewLine(s: String): String = s.map { c =>
      if (c == '\n') ' ' else c
    }
    for {
      cs <- nonEmpty(Arbitrary.arbitrary[String])
      t <- dec
    } yield CommentStatement(cs.map(cleanNewLine), t)
  }

  val argGen: Gen[(Identifier.Bindable, Option[TypeRef])] =
    for {
      arg <- bindIdentGen
      t <- Gen.option(typeRefGen)
    } yield (arg, t)

  def argToPat(arg: (Identifier.Bindable, Option[TypeRef])): Pattern.Parsed =
    arg match {
      case (bn, None)    => Pattern.Var(bn)
      case (bn, Some(t)) => Pattern.Annotation(Pattern.Var(bn), t)
    }

  def defGen[T](dec: Gen[T]): Gen[DefStatement[Pattern.Parsed, T]] =
    for {
      name <- bindIdentGen
      args <- smallNonEmptyList(smallNonEmptyList(argGen, 8), 20)
      tpes <- smallList(Gen.zip(typeRefVarGen, Gen.option(NTypeGen.genKind)))
      retType <- Gen.option(typeRefGen)
      body <- dec
    } yield DefStatement(
      name,
      NonEmptyList.fromList(tpes),
      args.map(_.map(argToPat)),
      retType,
      body
    )

  def genSpliceOrItem[A](
      spliceGen: Gen[A],
      itemGen: Gen[A]
  ): Gen[ListLang.SpliceOrItem[A]] =
    Gen.oneOf(
      spliceGen.map(ListLang.SpliceOrItem.Splice(_)),
      itemGen.map(ListLang.SpliceOrItem.Item(_))
    )

  def genListLangCons[A](
      spliceGen: Gen[A],
      itemGen: Gen[A]
  ): Gen[ListLang.Cons[ListLang.SpliceOrItem, A]] =
    Gen
      .choose(0, 5)
      .flatMap(Gen.listOfN(_, genSpliceOrItem(spliceGen, itemGen)))
      .map(ListLang.Cons(_))
  def genListLangDictCons[A](
      itemGen: Gen[A]
  ): Gen[ListLang.Cons[ListLang.KVPair, A]] =
    Gen
      .choose(0, 5)
      .flatMap(
        Gen.listOfN(
          _,
          Gen.zip(itemGen, itemGen).map { case (k, v) => ListLang.KVPair(k, v) }
        )
      )
      .map(ListLang.Cons(_))

  def genStringDecl(dec0: Gen[NonBinding]): Gen[Declaration.StringDecl] = {
    import Declaration.StringDecl

    val item =
      Gen.oneOf(
        Arbitrary.arbitrary[String].filter(_.length > 1).map { s =>
          StringDecl.Literal(emptyRegion, s)
        },
        dec0.map(StringDecl.StrExpr(_)),
        dec0.map(StringDecl.CharExpr(_))
      )

    def removeAdj[A](
        nea: NonEmptyList[A]
    )(fn: (A, A) => Boolean): NonEmptyList[A] =
      nea match {
        case NonEmptyList(a1, a2 :: tail) if fn(a1, a2) =>
          removeAdj(NonEmptyList(a2, tail))(fn)
        case NonEmptyList(a1, a2 :: tail) =>
          NonEmptyList(a1, removeAdj(NonEmptyList(a2, tail))(fn).toList)
        case ne1 => ne1
      }

    val res = for {
      sz <- Gen.choose(1, 4)
      lst <- Gen.listOfN(sz, item)
      nel = NonEmptyList.fromListUnsafe(lst)
      // make sure we don't have two adjacent strings
      nel1 = removeAdj(nel) {
        case (StringDecl.Literal(_, _), StringDecl.Literal(_, _)) => true
        case _                                                    => false
      }
    } yield Declaration.StringDecl(nel1)(using emptyRegion)

    res.filter {
      case Declaration.StringDecl(
            NonEmptyList(StringDecl.Literal(_, _), Nil)
          ) =>
        false
      case _ => true
    }
  }

  def listGen(dec0: Gen[NonBinding]): Gen[Declaration.ListDecl] = {
    lazy val filterFn: NonBinding => Boolean = {
      case Declaration.IfElse(_, _)                => false
      case Declaration.Match(_, _, _)              => false
      case Declaration.Lambda(_, body: NonBinding) => filterFn(body)
      case Declaration.Apply(f, args, _)           =>
        filterFn(f) && args.forall(filterFn)
      case _ => true
    }
    val dec = dec0.filter(filterFn)

    val cons = genListLangCons(dec, dec)

    // TODO we can't parse if since we get confused about it being a ternary expression
    val pat = genPattern(1, useUnion = true)
    val comp = Gen
      .zip(genSpliceOrItem(dec, dec), pat, dec, Gen.option(dec))
      .map { case (a, b, c0, _) =>
        val c = c0 match {
          case tern @ Declaration.Ternary(_, _, _) =>
            Declaration.Parens(tern)(using emptyRegion)
          case not => not
        }
        ListLang.Comprehension(a, b, c, None)
      }

    Gen.oneOf(cons, comp).map(Declaration.ListDecl(_)(using emptyRegion))
  }

  def dictGen(dec0: Gen[NonBinding]): Gen[Declaration.DictDecl] = {
    lazy val filterFn: NonBinding => Boolean = {
      case Declaration.Annotation(_, _)            => false
      case Declaration.IfElse(_, _)                => false
      case Declaration.Match(_, _, _)              => false
      case Declaration.ApplyOp(_, _, _)            => false
      case Declaration.Lambda(_, body: NonBinding) => filterFn(body)
      case Declaration.Apply(f, args, _)           =>
        filterFn(f) && args.forall(filterFn)
      case _ => true
    }
    val dec = dec0.filter(filterFn)

    val cons = genListLangDictCons(dec)

    // TODO we can't parse if since we get confused about it being a ternary expression
    val pat = genPattern(1, useUnion = true)
    val comp = Gen
      .zip(dec, dec, pat, dec, Gen.option(dec))
      .map { case (k, v, b, c0, _) =>
        val c = c0 match {
          case tern @ Declaration.Ternary(_, _, _) =>
            Declaration.Parens(tern)(using emptyRegion)
          case not => not
        }
        ListLang.Comprehension(ListLang.KVPair(k, v), b, c, None)
      }

    Gen.oneOf(cons, comp).map(Declaration.DictDecl(_)(using emptyRegion))
  }

  def varOrParens(decl: Gen[Declaration]): Gen[NonBinding] =
    decl.map {
      case n: Declaration.NonBinding =>
        n match {
          case v @ (Declaration.Var(_) | Declaration.Parens(_) |
              Declaration.Apply(_, _, _)) =>
            v
          case notVar => Declaration.Parens(notVar)(using emptyRegion)
        }
      case notVar => Declaration.Parens(notVar)(using emptyRegion)
    }

  def applyGen(decl: Gen[NonBinding]): Gen[NonBinding] =
    applyGen(varOrParens(decl), decl, Gen.oneOf(true, false))

  def isVar(d: Declaration): Boolean =
    d match {
      case Declaration.Var(_) => true
      case _                  => false
    }

  def applyGen(
      fnGen: Gen[NonBinding],
      arg: Gen[NonBinding],
      dotApplyGen: Gen[Boolean]
  ): Gen[Declaration.Apply] = {
    import Declaration._
    Gen.lzy(
      for {
        fn <- fnGen
        dotApply <- dotApplyGen
        useDot = dotApply && isVar(fn) // f.bar needs the fn to be a var
        argsGen =
          if (useDot) arg.map(NonEmptyList.one(_))
          else smallNonEmptyList(arg, 8)
        args <- argsGen
      } yield Apply(fn, args, ApplyKind.Parens)(using emptyRegion)
    ) // TODO this should pass if we use `foo.bar(a, b)` syntax
  }

  def applyOpGen(arg: Gen[NonBinding]): Gen[Declaration.ApplyOp] =
    Gen.zip(arg, opGen, arg).map { case (l, op, r) =>
      // a few types of things should be in raw ApplyOp, since
      // x -> x + y is parsed as x -> (x + y), for instance
      // also, parsing knows about precedence, but randomly
      // making expressions doesn't, so we have to wrap ApplyOp
      import Declaration._
      def protect(d: NonBinding): NonBinding =
        d match {
          case Var(_) | Apply(_, _, _) | Parens(_) | Matches(_, _) => d
          case _ => Parens(d)(using emptyRegion)
        }
      ApplyOp(protect(l), op, protect(r))
    }

  def bindGen[A, T](
      patGen: Gen[A],
      dec: Gen[NonBinding],
      tgen: Gen[T]
  ): Gen[BindingStatement[A, NonBinding, T]] =
    Gen
      .zip(patGen, dec, tgen)
      .map { case (b, value, in) =>
        BindingStatement(b, value, in)
      }

  def leftApplyGen(
      patGen: Gen[Pattern.Parsed],
      dec: Gen[NonBinding],
      bodyGen: Gen[Declaration]
  ): Gen[Declaration.LeftApply] =
    Gen
      .zip(patGen, dec, padding(bodyGen))
      .map { case (p, value, in) =>
        Declaration.LeftApply(p, emptyRegion, value, in)
      }

  def padding[T](tgen: Gen[T], min: Int = 0): Gen[Padding[T]] =
    Gen
      .zip(Gen.choose(min, 10), tgen)
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
    } yield Declaration.Lambda(args.map(Pattern.Var(_)), body)(using
      emptyRegion
    )

  def optIndent[A](genA: Gen[A]): Gen[OptIndent[A]] = {
    val indentation = Gen.choose(1, 10)
    indentation.flatMap { i =>
      // TODO support parsing if foo: bar
      // Gen.oneOf(
      padding(genA.map(Indented(i, _)), min = 1).map(OptIndent.notSame(_))
      // ,
      // bodyGen.map(Left(_): OptIndent[Declaration]))
    }
  }

  def ifElseGen(
      argGen0: Gen[NonBinding],
      bodyGen: Gen[Declaration]
  ): Gen[Declaration.IfElse] = {
    import Declaration._

    // args can't have raw annotations:
    val argGen = argGen0.map {
      case ann @ Annotation(_, _) => Parens(ann)(using emptyRegion)
      case notAnn                 => notAnn
    }

    val padBody = optIndent(bodyGen)
    val genIf: Gen[(NonBinding, OptIndent[Declaration])] =
      Gen.zip(argGen, padBody)

    Gen
      .zip(nonEmptyN(genIf, 2), padBody)
      .map { case (ifs, elsec) => IfElse(ifs, elsec)(using emptyRegion) }
  }

  def ternaryGen(argGen0: Gen[NonBinding]): Gen[Declaration.Ternary] = {
    import Declaration._

    val argGen = argGen0.map {
      case lam @ Lambda(_, _)      => Parens(lam)(using emptyRegion)
      case ife @ IfElse(_, _)      => Parens(ife)(using emptyRegion)
      case tern @ Ternary(_, _, _) => Parens(tern)(using emptyRegion)
      case matches @ Matches(_, _) => Parens(matches)(using emptyRegion)
      case m @ Match(_, _, _)      => Parens(m)(using emptyRegion)
      case not                     => not
    }
    Gen
      .zip(argGen, argGen, argGen)
      .map { case (t, c, f) => Ternary(t, c, f) }
  }

  def genStyle(args: List[Pattern.Parsed]): Gen[Pattern.StructKind.Style] =
    NonEmptyList.fromList(args) match {
      case None =>
        Gen.const(Pattern.StructKind.Style.TupleLike)
      case Some(NonEmptyList(h, tail)) =>
        def toArg(p: Pattern.Parsed): Gen[Pattern.StructKind.Style.FieldKind] =
          p match {
            case Pattern.Var(b: Identifier.Bindable) =>
              Gen.oneOf(
                Gen.const(Pattern.StructKind.Style.FieldKind.Implicit(b)),
                Gen
                  .oneOf(bindIdentGen, Gen.const(b))
                  .map(Pattern.StructKind.Style.FieldKind.Explicit(_))
              )
            case Pattern.Annotation(p, _) => toArg(p)
            case _                        =>
              // if we don't have a var, we can't omit the key
              bindIdentGen.map(Pattern.StructKind.Style.FieldKind.Explicit(_))
          }

        lazy val args = tail
          .foldLeft(
            toArg(h)
              .map(NonEmptyList.one)
          ) { case (args, a) =>
            Gen.zip(args, toArg(a)).map { case (args, a) =>
              NonEmptyList(a, args.toList)
            }
          }
          .map(_.reverse)

        Gen.oneOf(
          Gen.const(Pattern.StructKind.Style.TupleLike),
          Gen.lzy(args.map(Pattern.StructKind.Style.RecordLike(_)))
        )
    }

  def genStructKind(args: List[Pattern.Parsed]): Gen[Pattern.StructKind] =
    Gen.oneOf(
      Gen.const(Pattern.StructKind.Tuple),
      Gen.zip(consIdentGen, genStyle(args)).map { case (n, s) =>
        Pattern.StructKind.Named(n, s)
      },
      Gen.zip(consIdentGen, genStyle(args)).map { case (n, s) =>
        Pattern.StructKind.NamedPartial(n, s)
      }
    )

  def genPattern(depth: Int, useUnion: Boolean = true): Gen[Pattern.Parsed] =
    genPatternGen(
      genStructKind(_: List[Pattern.Parsed]),
      typeRefGen,
      depth,
      useUnion,
      useAnnotation = false
    )

  lazy val genStrPat: Gen[Pattern.StrPat] = {
    val recurse = Gen.lzy(genStrPat)

    val genPart: Gen[Pattern.StrPart] =
      Gen.oneOf(
        lowerIdent.map(Pattern.StrPart.LitStr(_)),
        bindIdentGen.map(Pattern.StrPart.NamedStr(_)),
        bindIdentGen.map(Pattern.StrPart.NamedChar(_)),
        Gen.const(Pattern.StrPart.WildStr),
        Gen.const(Pattern.StrPart.WildChar)
      )

    def isWild(p: Pattern.StrPart): Boolean =
      p match {
        case Pattern.StrPart.LitStr(_) | Pattern.StrPart.NamedChar(_) |
            Pattern.StrPart.WildChar =>
          false
        case _ => true
      }

    def makeValid(
        nel: NonEmptyList[Pattern.StrPart]
    ): NonEmptyList[Pattern.StrPart] =
      nel match {
        case NonEmptyList(_, Nil) => nel
        case NonEmptyList(h1, h2 :: t)
            if Pattern
              .StrPat(NonEmptyList.one(h1))
              .names
              .exists(Pattern.StrPat(NonEmptyList(h2, t)).names.toSet) =>
          makeValid(NonEmptyList(h2, t))
        case NonEmptyList(
              Pattern.StrPart.LitStr(h1),
              Pattern.StrPart.LitStr(h2) :: t
            ) =>
          makeValid(NonEmptyList(Pattern.StrPart.LitStr(h1 + h2), t))
        case NonEmptyList(h1, h2 :: t) =>
          val tail = makeValid(NonEmptyList(h2, t))
          if (isWild(tail.head) && isWild(h1)) {
            tail
          } else {
            h1 :: tail
          }
      }

    for {
      sz <- Gen.choose(1, 4) // don't get too giant, intersections blow up
      inner <- nonEmptyN(genPart, sz)
      p0 = Pattern.StrPat(makeValid(inner))
      notStr <- p0.toLiteralString.fold(Gen.const(p0))(_ => recurse)
    } yield notStr
  }

  def genPatternGen[N, T](
      genName: List[Pattern[N, T]] => Gen[N],
      genT: Gen[T],
      depth: Int,
      useUnion: Boolean,
      useAnnotation: Boolean
  ): Gen[Pattern[N, T]] = {
    val recurse =
      Gen.lzy(genPatternGen(genName, genT, depth - 1, useUnion, useAnnotation))
    val genVar = bindIdentGen.map(Pattern.Var(_))
    val genWild = Gen.const(Pattern.WildCard)
    val genLitPat = genLit.map(Pattern.Literal(_))

    if (depth <= 0) Gen.oneOf(genVar, genWild, genLitPat)
    else {
      val genNamed = Gen.zip(bindIdentGen, recurse).map { case (n, p) =>
        Pattern.Named(n, p)
      }
      val genTyped = Gen
        .zip(recurse, genT)
        .map { case (p, t) => Pattern.Annotation(p, t) }

      val genStruct = for {
        cnt <- Gen.choose(0, 6)
        args <- Gen.listOfN(cnt, recurse)
        nm <- genName(args)
      } yield Pattern.PositionalStruct(nm, args)

      def makeOneSplice(ps: List[Pattern.ListPart[Pattern[N, T]]]) = {
        val sz = ps.size
        if (sz == 0) Gen.const(ps)
        else
          Gen.choose(0, sz - 1).flatMap { idx =>
            val splice = Gen.oneOf(
              Gen.const(Pattern.ListPart.WildList),
              bindIdentGen.map(v => Pattern.ListPart.NamedList(v))
            )

            splice.map(v => ps.updated(idx, v))
          }
      }

      val genListItem: Gen[Pattern.ListPart[Pattern[N, T]]] =
        recurse.map(Pattern.ListPart.Item(_))

      val genList = Gen
        .choose(0, 5)
        .flatMap(Gen.listOfN(_, genListItem))
        .flatMap { ls =>
          Gen
            .oneOf(true, false)
            .flatMap {
              case true  => Gen.const(ls)
              case false => makeOneSplice(ls)
            }
        }
        .map(Pattern.ListPat(_))

      val genUnion = Gen
        .choose(0, 2)
        .flatMap(sz => Gen.zip(recurse, recurse, Gen.listOfN(sz, recurse)))
        .map { case (h0, h1, tail) =>
          Pattern.union(h0, h1 :: tail)
        }

      val tailGens: List[Gen[Pattern[N, T]]] =
        List(
          genVar,
          genWild,
          genNamed,
          genStrPat,
          genLitPat,
          genStruct,
          genList
        )

      val withU = if (useUnion) genUnion :: tailGens else tailGens
      val withT = (if (useAnnotation) genTyped :: withU else withU).toArray
      val len = withT.size
      Gen.choose(0, len - 1).flatMap(withT(_))
    }
  }

  def genCompiledPattern(
      depth: Int,
      useUnion: Boolean = true,
      useAnnotation: Boolean = true
  ): Gen[Pattern[(PackageName, Identifier.Constructor), rankn.Type]] =
    genPatternGen(
      (_: List[Pattern[(PackageName, Identifier.Constructor), rankn.Type]]) =>
        Gen.zip(packageNameGen, consIdentGen),
      NTypeGen.genDepth03,
      depth,
      useUnion = useUnion,
      useAnnotation = useAnnotation
    )

  val genRecursionKind: Gen[RecursionKind] =
    Gen.frequency(
      (20, Gen.const(RecursionKind.NonRecursive)),
      (1, Gen.const(RecursionKind.Recursive))
    )

  def matchGen(
      argGen0: Gen[NonBinding],
      bodyGen: Gen[Declaration]
  ): Gen[Declaration.Match] = {
    import Declaration._

    val padBody = optIndent(bodyGen)

    // args can't have raw annotations:
    val argGen = argGen0.map {
      case ann @ Annotation(_, _) => Parens(ann)(using emptyRegion)
      case notAnn                 => notAnn
    }

    val genCase: Gen[(Pattern.Parsed, OptIndent[Declaration])] =
      Gen.zip(genPattern(3), padBody)

    for {
      cnt <- Gen.choose(1, 2)
      kind <- genRecursionKind
      expr <- argGen
      cases <- optIndent(nonEmptyN(genCase, cnt))
    } yield Match(kind, expr, cases)(using emptyRegion)
  }

  def matchesGen(argGen0: Gen[NonBinding]): Gen[Declaration.Matches] =
    Gen.zip(argGen0, genPattern(3)).map { case (a, p) =>
      import Declaration._

      val fixa = a match {
        // matches binds tighter than all these
        case Lambda(_, _) | IfElse(_, _) | ApplyOp(_, _, _) | Match(_, _, _) |
            Ternary(_, _, _) =>
          Parens(a)(using emptyRegion)
        case _ => a
      }
      Matches(fixa, p)(using emptyRegion)
    }

  val genLit: Gen[Lit] = {
    val str = for {
      // q <- Gen.oneOf('\'', '"')
      // str <- Arbitrary.arbitrary[String]
      str <- lowerIdent // TODO
    } yield Lit.Str(str)

    val char = Gen.choose(0, 0xd7ff).map(i => Lit.Chr.fromCodePoint(i))

    val bi =
      Arbitrary.arbitrary[BigInt].map(bi => Lit.Integer(bi.bigInteger))
    Gen.oneOf(str, bi, char)
  }

  val identifierGen: Gen[Identifier] =
    Gen.oneOf(bindIdentGen, consIdentGen)

  val varGen: Gen[Declaration.Var] =
    bindIdentGen.map(Declaration.Var(_)(using emptyRegion))

  val consDeclGen: Gen[Declaration.Var] =
    consIdentGen.map(Declaration.Var(_)(using emptyRegion))

  val unnestedDeclGen: Gen[NonBinding] =
    Gen.frequency(
      (1, consDeclGen),
      (2, varGen),
      (1, genLit.map(Declaration.Literal(_)(using emptyRegion)))
    )

  def annGen(g: Gen[NonBinding]): Gen[Declaration.Annotation] = {
    import Declaration._
    Gen.zip(typeRefGen, g).map {
      case (t, r @ (Var(_) | Apply(_, _, _) | Parens(_))) =>
        Annotation(r, t)(using emptyRegion)
      case (t, wrap) =>
        Annotation(Parens(wrap)(using emptyRegion), t)(using emptyRegion)
    }
  }

  /** Generate a Declaration that can be parsed as a pattern
    */
  def patternDecl(depth: Int): Gen[NonBinding] = {
    import Declaration._
    val recur = Gen.lzy(patternDecl(depth - 1))

    val applyCons = applyGen(consDeclGen, recur, Gen.const(false))

    if (depth <= 0) unnestedDeclGen
    else
      Gen.frequency(
        (12, unnestedDeclGen),
        (2, applyCons),
        (1, recur.map(Parens(_)(using emptyRegion))),
        (1, annGen(recur)),
        (1, genListLangCons(varGen, recur).map(ListDecl(_)(using emptyRegion)))
      )
  }

  def simpleDecl(depth: Int): Gen[NonBinding] = {
    import Declaration._

    val unnested = unnestedDeclGen

    val recur = Gen.lzy(simpleDecl(depth - 1))
    if (depth <= 0) unnested
    else
      Gen.frequency(
        (13, unnested),
        (2, lambdaGen(recur)),
        (2, applyGen(recur)),
        (1, applyOpGen(recur)),
        (1, genStringDecl(recur)),
        (1, listGen(recur)),
        (1, dictGen(recur)),
        (1, annGen(recur)),
        (
          1,
          Gen
            .choose(0, 4)
            .flatMap(Gen.listOfN(_, recur))
            .map(TupleCons(_)(using emptyRegion))
        )
      )
  }

  def genRecordArg(dgen: Gen[NonBinding]): Gen[Declaration.RecordArg] =
    Gen
      .zip(bindIdentGen, Gen.option(dgen))
      .map {
        case (b, None)       => Declaration.RecordArg.Simple(b)
        case (b, Some(decl)) => Declaration.RecordArg.Pair(b, decl)
      }

  def genRecordDeclaration(
      dgen: Gen[NonBinding]
  ): Gen[Declaration.RecordConstructor] = {
    val args = for {
      tailSize <- Gen.choose(0, 4)
      args <- nonEmptyN(genRecordArg(dgen), tailSize)
    } yield args

    Gen.zip(consIdentGen, args).map { case (c, a) =>
      Declaration.RecordConstructor(c, a)(using emptyRegion)
    }
  }

  def genNonBinding(depth: Int): Gen[NonBinding] = {
    import Declaration._

    val unnested = unnestedDeclGen

    val recur = Gen.lzy(genDeclaration(depth - 1))
    val recNon = Gen.lzy(genNonBinding(depth - 1))
    if (depth <= 0) unnested
    else
      Gen.frequency(
        (14, unnested),
        (2, lambdaGen(recNon)),
        (2, applyGen(recNon)),
        (1, applyOpGen(simpleDecl(depth - 1))),
        (1, ifElseGen(recNon, recur)),
        (1, ternaryGen(recNon)),
        (1, genStringDecl(recNon)),
        (1, listGen(recNon)),
        (1, dictGen(recNon)),
        (1, matchGen(recNon, recur)),
        (1, matchesGen(recNon)),
        (
          1,
          Gen
            .choose(0, 4)
            .flatMap(Gen.listOfN(_, recNon))
            .map(TupleCons(_)(using emptyRegion))
        ),
        (1, genRecordDeclaration(recNon))
      )
  }

  def makeComment(c: CommentStatement[Padding[Declaration]]): Declaration = {
    import Declaration._
    c.on.padded match {
      case nb: NonBinding =>
        CommentNB(CommentStatement(c.message, Padding(c.on.lines, nb)))(
          using emptyRegion
        )
      case _ =>
        Comment(c)(using emptyRegion)
    }
  }

  def genDeclaration(depth: Int): Gen[Declaration] = {
    import Declaration._

    val unnested = unnestedDeclGen

    val pat: Gen[Pattern.Parsed] = bindIdentGen.map(Pattern.Var(_))
    // val pat = genPattern(0)

    val recur = Gen.lzy(genDeclaration(depth - 1))
    val recNon = Gen.lzy(genNonBinding(depth - 1))
    if (depth <= 0) unnested
    else
      Gen.frequency(
        (3, genNonBinding(depth)),
        (
          1,
          commentGen(padding(recur, 1)).map(makeComment)
        ), // make sure we have 1 space to prevent comments following each other
        (
          1,
          defGen(Gen.zip(optIndent(recur), padding(recur, 1)))
            .map(DefFn(_)(using emptyRegion))
        ),
        (
          1,
          bindGen(pat, recNon, padding(recur, 1))
            .map(Binding(_)(using emptyRegion))
        ),
        (1, leftApplyGen(pat, recNon, recur))
      )
  }

  implicit val shrinkDecl: Shrink[Declaration] =
    Shrink.withLazyList[Declaration] { d =>
      import Declaration._

      d match {
        case Annotation(t, _)   => t #:: LazyList.from(shrinkDecl.shrink(t))
        case Apply(fn, args, _) =>
          val next = fn #:: args.toList.to(LazyList)
          next.flatMap(shrinkDecl.shrink)
        case ao @ ApplyOp(left, _, right) =>
          left #:: ao.opVar #:: right #:: LazyList.empty
        case Binding(b) =>
          val next = b.value #:: b.in.padded #:: LazyList.empty
          next #::: next.flatMap(shrinkDecl.shrink)
        case DefFn(d) =>
          val (b, r) = d.result
          val inner = b.get #:: r.padded #:: LazyList.empty
          inner #::: inner.flatMap(shrinkDecl.shrink)
        case IfElse(ifCases, elseCase) =>
          elseCase.get #:: ifCases.toList.to(LazyList).map(_._2.get)
        case Ternary(t, c, f) =>
          val s = LazyList(t, c, f)
          s #::: s.flatMap(shrinkDecl.shrink)
        case LeftApply(_, _, r, b) =>
          // todo, we should really interleave shrinking r and b
          r #:: b.padded #:: LazyList.empty
        case Match(_, _, args) =>
          args.get.toList.to(LazyList).flatMap { case (_, decl) =>
            decl.get #:: LazyList.from(shrinkDecl.shrink(decl.get))
          }
        case Matches(a, _) =>
          a #:: LazyList.from(shrinkDecl.shrink(a))
        // the rest can't be shrunk
        case Comment(c)      => c.on.padded #:: LazyList.empty
        case CommentNB(c)    => c.on.padded #:: LazyList.empty
        case Lambda(_, body) => body #:: LazyList.empty
        case Literal(_)      => LazyList.empty
        case Parens(_)       =>
          // by removing parens we can make invalid
          // expressions
          LazyList.empty
        case TupleCons(Nil)       => LazyList.empty
        case TupleCons(h :: tail) =>
          h #:: TupleCons(tail)(using emptyRegion) #:: LazyList.from(
            shrinkDecl.shrink(TupleCons(tail)(using emptyRegion))
          )
        case Var(_)            => LazyList.empty
        case StringDecl(parts) =>
          parts.toList.to(LazyList).map {
            case StringDecl.StrExpr(nb)     => nb
            case StringDecl.CharExpr(nb)    => nb
            case StringDecl.Literal(r, str) =>
              Literal(Lit.Str(str))(using r)
          }
        case ListDecl(ListLang.Cons(items)) =>
          items.map(_.value).to(LazyList)
        case ListDecl(ListLang.Comprehension(a, _, c, d)) =>
          (a.value :: c :: d.toList).to(LazyList)
        case DictDecl(ListLang.Cons(items)) =>
          items.to(LazyList).flatMap(kv => LazyList(kv.key, kv.value))
        case DictDecl(ListLang.Comprehension(a, _, c, d)) =>
          (a.key :: a.value :: c :: d.toList).to(LazyList)
        case RecordConstructor(n, args) =>
          def head: LazyList[Declaration] =
            args.head match {
              case RecordArg.Pair(n, d) =>
                LazyList(Var(n)(using emptyRegion), d)
              case RecordArg.Simple(n) =>
                LazyList(Var(n)(using emptyRegion))
            }

          def tailStream(
              of: NonEmptyList[RecordArg]
          ): LazyList[NonEmptyList[RecordArg]] =
            NonEmptyList.fromList(of.tail) match {
              case None           => LazyList.empty
              case Some(tailArgs) =>
                tailArgs #:: tailStream(tailArgs) #::: tailStream(
                  NonEmptyList(of.head, tailArgs.tail)
                )
            }

          Var(n)(using emptyRegion) #::
            head #:::
            tailStream(args).map(
              RecordConstructor(n, _)(using emptyRegion): Declaration
            ) // type annotation for scala 2.11
      }
    }

  def interleave[A](s1: LazyList[A], s2: LazyList[A]): LazyList[A] =
    if (s1.isEmpty) s2
    else if (s2.isEmpty) s1
    else {
      s1.head #:: interleave(s2, s1.tail)
    }

  def interleaveAll[A](ss: List[LazyList[A]]): LazyList[A] =
    ss match {
      case Nil        => LazyList.empty
      case one :: Nil => one
      case twoOrMore  =>
        val (l, r) = twoOrMore.splitAt(twoOrMore.size / 2)
        interleave(interleaveAll(l), interleaveAll(r))
    }
  // treat the list like a product and shrink each position
  def shrinkOne[A: Shrink](list: List[A]): LazyList[List[A]] =
    interleaveAll((0 until list.size).toList.map { idx =>
      val aIdx = list(idx)
      LazyList
        .from(implicitly[Shrink[A]].shrink(aIdx))
        .map { a =>
          list.updated(idx, a)
        }
    })

  def dropItemList[A](list: List[A]): LazyList[List[A]] =
    list match {
      case Nil | _ :: Nil => LazyList.empty
      case twoOrMore      =>
        (0 until twoOrMore.size).to(LazyList).map { idx =>
          list.take(idx) ::: list.drop(idx + 1)
        }
    }

  implicit def shrinkPattern[N, T]: Shrink[Pattern[N, T]] = {
    lazy val res: Shrink[Pattern[N, T]] =
      Shrink.withLazyList { p =>
        p match {
          case Pattern.WildCard => LazyList.empty
          case Pattern.Var(_)   => Pattern.WildCard #:: LazyList.empty
          case Pattern.Annotation(pattern, _)      => pattern #:: LazyList.empty
          case Pattern.Named(_, pat)               => pat #:: LazyList.empty
          case Pattern.PositionalStruct(n, params) =>
            // shrink all the params
            shrinkOne(params)(using res).map(Pattern.PositionalStruct(n, _))
          case Pattern.Literal(Lit.Str(s)) =>
            LazyList
              .from(implicitly[Shrink[String]].shrink(s))
              .map(s => Pattern.Literal(Lit(s)))
          case Pattern.Literal(Lit.Integer(s)) =>
            LazyList
              .from(implicitly[Shrink[BigInt]].shrink(BigInt(s)))
              .map(s => Pattern.Literal(Lit.Integer(s.bigInteger)))
          case Pattern.Literal(_)  => LazyList.empty
          case Pattern.ListPat(ls) =>
            if (ls.isEmpty) LazyList.empty
            else (Pattern.ListPat(ls.tail) #:: LazyList.empty)
          case Pattern.StrPat(ls) =>
            if (ls.tail.isEmpty) LazyList.empty
            else
              (Pattern.StrPat(
                NonEmptyList.fromListUnsafe(ls.tail)
              ) #:: LazyList.empty)
          case u @ Pattern.Union(_, _) =>
            val flat = Pattern.flatten(u).toList
            val sameLen =
              shrinkOne[Pattern[N, T]](flat)(using res)
                .map { us =>
                  Pattern.union(us.head, us.tail)
                }
            // unions have 2 or more, so this won't throw
            val oneLess = dropItemList(flat).map { smaller =>
              Pattern.union(smaller.head, smaller.tail)
            }

            interleave(oneLess, sameLen)
        }
      }

    res
  }

  implicit val shrinkStmt: Shrink[Statement] =
    Shrink.withLazyList { s =>
      import Statement._

      s match {
        case Bind(bs @ BindingStatement(_, d, _)) =>
          LazyList.from(shrinkDecl.shrink(d)).collect { case sd: NonBinding =>
            Bind(bs.copy(value = sd))(emptyRegion)
          }
        case Def(ds) =>
          val body = ds.result
          body
            .traverse(d => LazyList.from(shrinkDecl.shrink(d)))
            .map { bod =>
              Def(ds.copy(result = bod))(emptyRegion)
            }
        case _ => LazyList.empty
      }
    }

  val constructorGen: Gen[
    (Identifier.Constructor, List[(Identifier.Bindable, Option[TypeRef])])
  ] =
    for {
      name <- consIdentGen
      args <- smallList(argGen)
    } yield (name, args)

  val genTypeArgs: Gen[List[(TypeRef.TypeVar, Option[Kind.Arg])]] =
    smallList(Gen.zip(typeRefVarGen, Gen.option(NTypeGen.genKindArg)))
      .map(_.distinctBy(_._1))

  val genStruct: Gen[Statement] =
    Gen
      .zip(constructorGen, genTypeArgs)
      .map { case ((name, args), ta) =>
        Statement.Struct(name, NonEmptyList.fromList(ta), args)(emptyRegion)
      }

  val genExternalStruct: Gen[Statement] =
    for {
      name <- consIdentGen
      args <- genTypeArgs
    } yield Statement.ExternalStruct(name, args)(emptyRegion)

  val genExternalDef: Gen[Statement] =
    for {
      name <- bindIdentGen
      tas0 <- Gen.option(
        smallList(Gen.zip(typeRefVarGen, Gen.option(NTypeGen.genKind)))
      )
      argc <- Gen.choose(0, 5)
      argG = Gen.zip(bindIdentGen, typeRefGen)
      args <- Gen.listOfN(argc, argG)
      tas = if (args.isEmpty) None else tas0
      res <- typeRefGen
    } yield Statement.ExternalDef(
      name,
      tas.flatMap(NonEmptyList.fromList(_)),
      args,
      res
    )(emptyRegion)

  val genEnum: Gen[Statement] =
    for {
      name <- consIdentGen
      ta <- genTypeArgs
      consc <- Gen.choose(1, 5)
      cons <- optIndent(nonEmptyN(constructorGen, consc))
    } yield Statement.Enum(name, NonEmptyList.fromList(ta), cons)(emptyRegion)

  def genStatement(depth: Int): Gen[Statement] = {
    val decl = genDeclaration(depth)
    val nonB = genNonBinding(depth)
    // TODO make more powerful
    val pat: Gen[Pattern.Parsed] = genPattern(1)
    Gen.frequency(
      (
        1,
        bindGen(pat, nonB, Gen.const(())).map(Statement.Bind(_)(emptyRegion))
      ),
      (1, commentGen(Gen.const(())).map(Statement.Comment(_)(emptyRegion))),
      (1, defGen(optIndent(decl)).map(Statement.Def(_)(emptyRegion))),
      (1, genStruct),
      (1, genExternalStruct),
      (1, genExternalDef),
      (1, genEnum),
      (
        1,
        padding(Gen.const(()), 1)
          .map(Statement.PaddingStatement(_)(emptyRegion))
      )
    )
  }

  def genStatements(depth: Int, maxLength: Int): Gen[List[Statement]] = {
    import Statement._

    /*
     * repeated paddings or comments will be combined by parsing
     * and will never be seen
     */
    def combineDuplicates(stmts: List[Statement]): List[Statement] =
      stmts match {
        case Nil      => Nil
        case h :: Nil => h :: Nil
        case PaddingStatement(Padding(a, _)) :: PaddingStatement(
              Padding(b, _)
            ) :: rest =>
          combineDuplicates(
            PaddingStatement(Padding(a + b, ()))(emptyRegion) :: rest
          )
        case Comment(CommentStatement(lines1, _)) :: Comment(
              CommentStatement(lines2, _)
            ) :: rest =>
          combineDuplicates(
            Comment(CommentStatement(lines1 ::: lines2, ()))(
              emptyRegion
            ) :: rest
          )
        case h1 :: rest =>
          h1 :: combineDuplicates(rest)
      }

    for {
      cnt <- Gen.choose(0, maxLength)
      lst <- Gen.listOfN(cnt, Generators.genStatement(depth))
    } yield combineDuplicates(lst)
  }

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
      (h, tail) <- Gen.listOfN(importCount, importedNameGen).map {
        case Nil => sys.error("got an empty list, but import count min is 1")
        case h :: tail => (h, tail)
      }
    } yield Import(p, NonEmptyList(h, tail))

  val exportedNameGen: Gen[ExportedName[Unit]] =
    Gen.oneOf(
      bindIdentGen.map(ExportedName.Binding(_, ())),
      consIdentGen.map(ExportedName.TypeName(_, ())),
      consIdentGen.map(ExportedName.Constructor(_, ()))
    )

  def smallList[A](g: Gen[A]): Gen[List[A]] =
    Gen.choose(0, 8).flatMap(Gen.listOfN(_, g))

  def smallNonEmptyList[A](g: Gen[A], maxLen: Int): Gen[NonEmptyList[A]] =
    // bias to small numbers
    Gen
      .geometric(2.0)
      .flatMap {
        case n if n <= 0 => g.map(NonEmptyList.one)
        case n           =>
          Gen
            .zip(g, Gen.listOfN((n - 1) min (maxLen - 1), g))
            .map { case (h, t) => NonEmptyList(h, t) }
      }

  def smallDistinctByList[A, B](g: Gen[A])(fn: A => B): Gen[List[A]] =
    Gen
      .choose(0, 8)
      .flatMap(Gen.listOfN(_, g))
      .map(graph.Tree.distinctBy(_)(fn))

  def packageGen(depth: Int): Gen[Package.Parsed] =
    for {
      p <- packageNameGen
      ec <- Gen.choose(0, 10)
      imports <- smallList(importGen)
      exports <- Gen.listOfN(ec, exportedNameGen)
      body <- genStatements(depth, 10)
    } yield Package(p, imports, exports, body)

  def genDefinedType[A](
      p: PackageName,
      inner: Gen[A],
      genType: Gen[rankn.Type]
  ): Gen[rankn.DefinedType[A]] =
    for {
      t <- typeNameGen
      paramKeys <- smallList(NTypeGen.genBound).map(_.distinct)
      params <- paramKeys.traverse(p => inner.map((p, _)))
      genCons: Gen[rankn.ConstructorFn] =
        for {
          cons <- consIdentGen
          ps <- smallList(Gen.zip(bindIdentGen, genType))
        } yield rankn.ConstructorFn(cons, ps)
      cons0 <- smallList(genCons)
      cons = cons0.map(cf => (cf.name, cf)).toMap.values.toList
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
      case Nil  => bind(NTypeGen.genDepth03)
      case dts0 =>
        // only make one of each type
        val dts = dts0.map(dt => (dt.name.ident, dt)).toMap.values.toList

        val b = bind(
          Gen.oneOf(NTypeGen.genDepth03, Gen.oneOf(dts).map(_.toTypeTyConst))
        )
        val genExpT = Gen
          .oneOf(dts)
          .map { dt =>
            ExportedName.TypeName(dt.name.ident, Referant.DefinedT(dt))
          }

        dts.filter(_.constructors.nonEmpty) match {
          case Nil      => Gen.oneOf(b, genExpT)
          case nonEmpty =>
            val c = for {
              dt <- Gen.oneOf(nonEmpty)
              cf <- Gen.oneOf(dt.constructors)
            } yield ExportedName.Constructor(
              cf.name,
              Referant.Constructor(dt, cf)
            )
            Gen.oneOf(b, genExpT, c)
        }
    }
  }

  val interfaceGen: Gen[Package.Interface] =
    for {
      p <- packageNameGen
      te <- typeEnvGen(
        p,
        Gen.oneOf(
          Kind.Type.co,
          Kind.Type.phantom,
          Kind.Type.contra,
          Kind.Type.in
        )
      )
      exs0 <- smallList(exportGen(te))
      exs = exs0
        .map(ex => (ex.name, ex))
        .toMap
        .values
        .toList // don't duplicate exported names
    } yield Package(p, Nil, exs, ())

  /** This is a totally random, and not well typed expression. It is suitable
    * for some tests, but it is not a valid output of a typechecking process
    */
  def genTypedExpr[A](
      genTag: Gen[A],
      depth: Int,
      typeGen: Gen[rankn.Type]
  ): Gen[TypedExpr[A]] = {
    val recurse = Gen.lzy(genTypedExpr(genTag, depth - 1, typeGen))
    val lit = Gen.zip(genLit, NTypeGen.genDepth03, genTag).map {
      case (l, tpe, tag) => TypedExpr.Literal(l, tpe, tag)
    }
    // only literal doesn't recurse
    if (depth <= 0) lit
    else {
      val genGeneric =
        Gen
          .zip(
            Generators.nonEmpty(Gen.zip(NTypeGen.genBound, NTypeGen.genKind)),
            recurse
          )
          .map { case (vs, t) => TypedExpr.forAll(vs, t) }

      val ann =
        Gen
          .zip(recurse, typeGen)
          .map { case (te, tpe) => TypedExpr.Annotation(te, tpe) }

      val lam =
        Gen
          .zip(
            smallNonEmptyList(Gen.zip(bindIdentGen, typeGen), 8),
            recurse,
            genTag
          )
          .map { case (args, res, tag) =>
            TypedExpr.AnnotatedLambda(args, res, tag)
          }

      val localGen =
        Gen
          .zip(bindIdentGen, typeGen, genTag)
          .map { case (n, t, tag) => TypedExpr.Local(n, t, tag) }

      val globalGen =
        Gen
          .zip(packageNameGen, identifierGen, typeGen, genTag)
          .map { case (p, n, t, tag) => TypedExpr.Global(p, n, t, tag) }

      val app =
        Gen
          .zip(recurse, smallNonEmptyList(recurse, 8), typeGen, genTag)
          .map { case (fn, args, tpe, tag) =>
            TypedExpr.App(fn, args, tpe, tag)
          }

      val let =
        Gen
          .zip(
            bindIdentGen,
            recurse,
            recurse,
            genRecursionKind,
            genTag
          )
          .map { case (n, ex, in, rec, tag) =>
            TypedExpr.Let(n, ex, in, rec, tag)
          }

      val matchGen =
        Gen
          .zip(
            recurse,
            Gen
              .choose(1, 4)
              .flatMap(
                nonEmptyN(Gen.zip(genCompiledPattern(depth), recurse), _)
              ),
            genTag
          )
          .map { case (arg, branches, tag) =>
            TypedExpr.Match(arg, branches, tag)
          }

      Gen.oneOf(
        genGeneric,
        ann,
        lam,
        localGen,
        globalGen,
        app,
        let,
        lit,
        matchGen
      )
    }
  }

  def traverseGen[F[_]: Traverse, A, B](fa: F[A])(fn: A => Gen[B]): Gen[F[B]] =
    fa.traverse(fn)

  def shuffle[A](as: List[A]): Gen[List[A]] =
    as match {
      case (Nil | (_ :: Nil)) => Gen.const(as)
      case a :: b :: Nil      =>
        Gen.oneOf(as, b :: a :: Nil)
      case _ =>
        val size = as.size
        def loop(idx: Int): Gen[List[Int]] =
          if (idx >= size) Gen.const(Nil)
          else
            Gen
              .zip(Gen.choose(idx, size - 1), loop(idx + 1))
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

  def genOnePackage[A](
      genA: Gen[A],
      existing: Map[PackageName, Package.Typed[A]]
  ): Gen[Package.Typed[A]] = {
    val genDeps: Gen[Map[PackageName, Package.Typed[A]]] =
      Gen.frequency(
        (
          5,
          Gen.const(Map.empty)
        ), // usually have no deps, otherwise the graph gets enormous
        (1, shuffle(existing.toList).map(_.take(2).toMap))
      )

    def impFromExp(
        exp: List[(Package.Interface, ExportedName[Referant[Kind.Arg]])]
    ): Gen[List[Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]]] =
      exp
        .groupBy(_._1)
        .toList
        .traverse { case (p, exps) =>
          val genImps
              : Gen[List[ImportedName[NonEmptyList[Referant[Kind.Arg]]]]] =
            exps
              .groupBy(_._2.name)
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

    val genImports: Gen[
      List[Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]]
    ] =
      genDeps.flatMap { packs =>
        val exps: List[(Package.Interface, ExportedName[Referant[Kind.Arg]])] =
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

    def definedTypesFromImp(
        i: Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
    ): List[rankn.Type.Const] =
      i.items.toList.flatMap { in =>
        in.tag.toList.flatMap {
          case Referant.DefinedT(dt)       => dt.toTypeConst :: Nil
          case Referant.Constructor(dt, _) => dt.toTypeConst :: Nil
          case Referant.Value(_)           => Nil
        }
      }

    def genTypeEnv(
        pn: PackageName,
        imps: List[Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]]
    ): StateT[Gen, (rankn.TypeEnv[Kind.Arg], Set[Identifier.Bindable]), Unit] =
      StateT
        .get[Gen, (rankn.TypeEnv[Kind.Arg], Set[Identifier.Bindable])]
        .flatMap { case (te, extDefs) =>
          StateT
            .liftF(Gen.choose(0, 9))
            .flatMap {
              case 0 =>
                // 1 in 10 chance of stopping
                StateT.pure[
                  Gen,
                  (rankn.TypeEnv[Kind.Arg], Set[Identifier.Bindable]),
                  Unit
                ](())
              case _ =>
                // add something:
                val tyconsts =
                  te.allDefinedTypes.map(_.toTypeConst) ++
                    imps.flatMap(definedTypesFromImp)
                val theseTypes = NTypeGen.genDepth(
                  4,
                  if (tyconsts.isEmpty) None else Some(Gen.oneOf(tyconsts))
                )
                val genV: Gen[Kind.Arg] =
                  Gen.oneOf(
                    Kind.Type.co,
                    Kind.Type.contra,
                    Kind.Type.in,
                    Kind.Type.phantom
                  )
                val genDT = genDefinedType(pn, genV, theseTypes)
                val genEx: Gen[(Identifier.Bindable, rankn.Type)] =
                  Gen.zip(bindIdentGen, theseTypes)

                // we can do one of the following:
                // 1: add an external value
                // 2: add a defined type
                StateT
                  .liftF(
                    Gen.frequency(
                      (
                        5,
                        genDT.map { dt =>
                          (te.addDefinedTypeAndConstructors(dt), extDefs)
                        }
                      ),
                      (
                        1,
                        genEx.map { case (b, t) =>
                          (te.addExternalValue(pn, b, t), extDefs + b)
                        }
                      )
                    )
                  )
                  .flatMap(StateT.set(_))
            }
        }

    def genLets(
        te: rankn.TypeEnv[Kind.Arg],
        exts: Set[Identifier.Bindable]
    ): Gen[List[(Identifier.Bindable, RecursionKind, TypedExpr[A])]] = {
      val allTC = te.allDefinedTypes.map(_.toTypeConst)
      val theseTypes = NTypeGen.genDepth(
        4,
        if (allTC.isEmpty) None else Some(Gen.oneOf(allTC))
      )
      val oneLet = Gen.zip(
        bindIdentGen.filter(b => !exts(b)),
        genRecursionKind,
        genTypedExpr(genA, 4, theseTypes)
      )

      Gen.choose(0, 6).flatMap(Gen.listOfN(_, oneLet))
    }

    def genProg(
        pn: PackageName,
        imps: List[Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]]
    ): Gen[Program[rankn.TypeEnv[Kind.Arg], TypedExpr[A], Any]] =
      genTypeEnv(pn, imps)
        .runS((rankn.TypeEnv.empty, Set.empty))
        .flatMap { case (te, b) =>
          genLets(te, b).map(Program(te, _, b.toList.sorted, ()))
        }

    /*
     * Exports are types, constructors, or values
     */
    def genExports(
        pn: PackageName,
        p: Program[rankn.TypeEnv[Kind.Arg], TypedExpr[A], Any]
    ): Gen[List[ExportedName[Referant[Kind.Arg]]]] = {
      def expnames: List[ExportedName[Referant[Kind.Arg]]] =
        p.lets.map { case (n, _, te) =>
          ExportedName.Binding(n, Referant.Value(te.getType))
        }
      def exts: List[ExportedName[Referant[Kind.Arg]]] =
        p.externalDefs.flatMap { n =>
          p.types.getValue(pn, n).map { t =>
            ExportedName.Binding(n, Referant.Value(t))
          }
        }

      def cons: List[ExportedName[Referant[Kind.Arg]]] =
        p.types.allDefinedTypes.flatMap { dt =>
          if (dt.packageName == pn) {
            val dtex =
              ExportedName.TypeName(dt.name.ident, Referant.DefinedT(dt))
            val cons = dt.constructors.map { cf =>
              ExportedName.Constructor(cf.name, Referant.Constructor(dt, cf))
            }

            dtex :: cons
          } else Nil
        }

      for {
        cnt <- Gen.choose(0, 5)
        lst <- shuffle(expnames ::: exts ::: cons)
      } yield lst.take(cnt)
    }

    for {
      pn <- packageNameGen
      // we can't reuse package names
      if !existing.contains(pn)
      imps0 <- genImports
      impMap = ImportMap.fromImports(imps0)((_, _) => ImportMap.Unify.Error)._2
      imps = impMap.toList(using Package.orderByName)
      prog <- genProg(pn, imps)
      exps <- genExports(pn, prog)
    } yield Package(pn, imps, exps, (prog, impMap))
  }

  def genPackagesSt[A](
      genA: Gen[A],
      maxSize: Int
  ): StateT[Gen, Map[PackageName, Package.Typed[A]], Unit] =
    StateT
      .get[Gen, Map[PackageName, Package.Typed[A]]]
      .flatMap { m =>
        if (m.size >= maxSize) StateT.pure(())
        else {
          // make one more and try again
          for {
            p <- StateT
              .liftF[Gen, Map[PackageName, Package.Typed[A]], Package.Typed[A]](
                genOnePackage(genA, m)
              )
            _ <- StateT.set[Gen, Map[PackageName, Package.Typed[A]]](
              m.updated(p.name, p)
            )
            _ <- genPackagesSt(genA, maxSize)
          } yield ()
        }
      }

  def genPackage[A](
      genA: Gen[A],
      maxSize: Int
  ): Gen[Map[PackageName, Package.Typed[A]]] =
    genPackagesSt(genA, maxSize).runS(Map.empty)

  object Exprs {
    def gen[A](genA: Gen[A], depth: Int): Gen[Expr[A]] = {
      import Expr._

      val roots: Gen[Expr[A]] =
        Gen.frequency(
          (1, Gen.zip(genLit, genA).map { case (l, t) => Literal(l, t) }),
          (1, Gen.zip(bindIdentGen, genA).map { case (b, t) => Local(b, t) }),
          (
            1,
            Gen.zip(NTypeGen.packageNameGen, identifierGen, genA).map {
              case (p, i, t) => Global(p, i, t)
            }
          )
        )

      if (depth <= 0) roots
      else {
        val recur = Gen.lzy(gen(genA, depth - 1))
        Gen.frequency(
          (1, roots),
          (
            1,
            Gen.zip(recur, NTypeGen.genDepth03, genA).map { case (e, t, tag) =>
              Annotation(e, t, tag)
            }
          ),
          (
            1,
            Gen
              .zip(
                smallNonEmptyList(
                  Gen.zip(NTypeGen.genBound, NTypeGen.genKind),
                  4
                ),
                recur
              )
              .map { case (ts, in) =>
                Generic(ts, in)
              }
          ),
          (
            2,
            Gen.zip(recur, smallNonEmptyList(recur, 5), genA).map {
              case (fn, as, t) => App(fn, as, t)
            }
          ),
          (
            2,
            Gen
              .zip(
                smallNonEmptyList(
                  Gen.zip(bindIdentGen, Gen.option(NTypeGen.genDepth03)),
                  4
                ),
                recur,
                genA
              )
              .map { case (as, e, t) =>
                Lambda(as, e, t)
              }
          ),
          (
            4,
            Gen
              .zip(
                bindIdentGen,
                recur,
                recur,
                genRecursionKind,
                genA
              )
              .map { case (a, e, in, r, t) => Let(a, e, in, r, t) }
          ),
          (
            1,
            Gen
              .zip(
                recur,
                smallNonEmptyList(Gen.zip(genCompiledPattern(4), recur), 3),
                genA
              )
              .map { case (a, bs, t) => Match(a, bs, t) }
          )
        )
      }
    }
  }
}
