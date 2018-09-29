package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen}

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

  val keyWords = Set("if", "ffi", "match", "struct", "enum", "else", "elif", "def", "external", "package", "import", "export", "forall")

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

    Gen.frequency(
      (4, tvar),
      (4, tname),
      (1, Gen.zip(Gen.lzy(typeRefGen), Gen.lzy(typeRefGen)).map { case (a, b) => TypeArrow(a, b) }),
      (1, tLambda),
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
      args <- nonEmpty(argGen)
      retType <- Gen.option(typeRefGen)
      body <- dec
    } yield DefStatement(name, args, retType, body)

  def applyGen(dec: Gen[Declaration]): Gen[Declaration] = {
    import Declaration._
    Gen.lzy(for {
      fn <- dec
      varfnParens = fn match { case v@Declaration.Var(_) => (true, v); case nonV => (false, Parens(nonV)(emptyRegion)) }
      (isVar, fnParens) = varfnParens
      dotApply <- Gen.oneOf(true, false)
      useDot = dotApply && isVar // f.bar needs the fn to be a var
      argsGen = if (useDot) dec.map(NonEmptyList(_, Nil)) else nonEmpty(dec)
      args <- argsGen
    } yield Apply(fnParens, args, false)(emptyRegion)) // TODO this should pass if we use `foo.bar(a, b)` syntax
  }

  def bindGen[T](dec: Gen[Declaration], tgen: Gen[T]): Gen[BindingStatement[T]] =
    for {
      b <- lowerIdent
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

  def ifElseGen(bodyGen: Gen[Declaration]): Gen[Declaration.IfElse] = {
    import Declaration._

    val indentation = Gen.choose(1, 10)
    indentation.flatMap { i =>

      val padBody = padding(bodyGen.map(Indented(i, _)))

      val genIf: Gen[(Declaration, Padding[Indented[Declaration]])] =
        Gen.zip(bodyGen, padBody)

      Gen.zip(nonEmptyN(genIf, 2), padBody)
        .map { case (ifs, elsec) => IfElse(ifs, elsec)(emptyRegion) }
    }
  }

  def matchGen(bodyGen: Gen[Declaration]): Gen[Declaration.Match] = {
    import Declaration._

    val indentation = Gen.choose(1, 10)
    indentation.flatMap { i =>

      val padBody = padding(bodyGen.map(Indented(i, _)))

      val genPattern = for {
        nm <- upperIdent
        cnt <- Gen.choose(0, 6)
        args <- Gen.listOfN(cnt, Gen.option(lowerIdent))
        argPat = args.map {
          case None => Pattern.WildCard
          case Some(v) => Pattern.Var(v)
        }
      } yield Pattern.PositionalStruct(nm, argPat)

      val genCase: Gen[(Pattern[String], Padding[Indented[Declaration]])] =
        Gen.zip(genPattern, padBody)

      val padIndCase = padding(genCase.map(Indented(i, _)))

      for {
        cnt <- Gen.choose(1, 2)
        expr <- bodyGen
        cases <- nonEmptyN(padIndCase, cnt)
      } yield Match(expr, cases)(emptyRegion)
    }
  }

  def genDeclaration(depth: Int): Gen[Declaration] = {
    import Declaration._

    val str = for {
      q <- Gen.oneOf('\'', '"')
      //str <- Arbitrary.arbitrary[String]
      str <- lowerIdent // TODO
    } yield LiteralString(str, q)(emptyRegion)

    val unnested = Gen.oneOf(
      lowerIdent.map(Var(_)(emptyRegion)),
      upperIdent.map(Constructor(_)(emptyRegion)),
      //Arbitrary.arbitrary[BigInt].map { bi => LiteralInt(bi.toString) }, // TODO enable bigint
      Arbitrary.arbitrary[Int].map { bi => LiteralInt(bi.toString)(emptyRegion) },
      str)

    val recur = Gen.lzy(genDeclaration(depth - 1))
    if (depth <= 0) unnested
    else Gen.frequency(
      (12, unnested),
      (2, commentGen(padding(recur, 1)).map(Comment(_)(emptyRegion))), // make sure we have 1 space to prevent comments following each other
      (2, defGen(Gen.zip(padding(indented(recur)), padding(recur))).map(DefFn(_)(emptyRegion))),
      (2, lambdaGen(recur)),
      (2, applyGen(recur)),
      (2, bindGen(recur, padding(recur, 1)).map(Binding(_)(emptyRegion))),
      (1, ifElseGen(recur)),
      (1, matchGen(recur))
    )
  }

  val constructorGen: Gen[(String, List[(String, Option[TypeRef])])] =
    for {
      name <- upperIdent
      argc <- Gen.choose(0, 5)
      args <- Gen.listOfN(argc, argGen)
    } yield (name, args)


  def genStruct(tail: Gen[Statement]): Gen[Statement] =
    Gen.zip(constructorGen, padding(tail))
      .map { case ((name, args), rest) =>
        Statement.Struct(name, args, rest)
      }

  def genExternalStruct(tail: Gen[Statement]): Gen[Statement] =
    for {
      name <- upperIdent
      argc <- Gen.choose(0, 5)
      args <- Gen.listOfN(argc, lowerIdent)
      rest <- padding(tail)
    } yield Statement.ExternalStruct(name, args.map(TypeRef.TypeVar(_)), rest)

  def genExternalDef(tail: Gen[Statement]): Gen[Statement] =
    for {
      name <- lowerIdent
      argc <- Gen.choose(0, 5)
      argG = Gen.zip(lowerIdent, typeRefGen)
      args <- Gen.listOfN(argc, argG)
      res <- typeRefGen
      rest <- padding(tail)
    } yield Statement.ExternalDef(name, args, res, rest)

  def genEnum(tail: Gen[Statement]): Gen[Statement] =
    for {
      name <- upperIdent
      consc <- Gen.choose(1, 5)
      i <- Gen.choose(1, 16)
      padBody = padding(constructorGen.map(Indented(i, _)))
      cons <- nonEmptyN(padBody, consc)
      rest <- padding(tail)
    } yield Statement.Enum(name, cons, rest)

  val genStatement: Gen[Statement] = {
    val recur = Gen.lzy(genStatement)
    val decl = genDeclaration(5)
    Gen.frequency(
      (1, bindGen(decl, padding(recur)).map(Statement.Bind(_))),
      (1, commentGen(padding(recur)
        .map {
          case Padding(0, c@Statement.Comment(_)) => Padding[Statement](1, c) // make sure not two back to back comments
          case other => other
        }).map(Statement.Comment(_))),
      (1, defGen(Gen.zip(padding(indented(decl)), padding(recur))).map(Statement.Def(_))),
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

  val packageGen: Gen[Package.Parsed] =
    for {
      p <- packageNameGen
      ic <- Gen.choose(0, 8)
      ec <- Gen.choose(0, 10)
      imports <- Gen.listOfN(ic, importGen)
      exports <- Gen.listOfN(ec, exportedNameGen)
      body <- genStatement
    } yield Package(p, imports, exports, body.toProgram(p))
}
