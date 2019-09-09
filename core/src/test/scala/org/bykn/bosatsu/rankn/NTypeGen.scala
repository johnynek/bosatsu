package org.bykn.bosatsu
package rankn

import cats.data.NonEmptyList
import org.scalacheck.Gen

object NTypeGen {
  val lower: Gen[Char] = Gen.oneOf('a' to 'z')
  val upper: Gen[Char] = Gen.oneOf('A' to 'Z')
  val num: Gen[Char] = Gen.oneOf('0' to '9')
  val identC: Gen[Char] = Gen.frequency((10, lower), (1, upper), (1, num))

  val upperIdent: Gen[String] =
    for {
      c <- upper
      cnt <- Gen.choose(0, 10)
      rest <- Gen.listOfN(cnt, identC)
    } yield (c :: rest).mkString

  val consIdentGen: Gen[Identifier.Constructor] =
    upperIdent.map { n => Identifier.Constructor(n) }

  val typeNameGen: Gen[TypeName] =
    consIdentGen.map(TypeName(_))

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

  val packageNameGen: Gen[PackageName] =
    for {
      pc <- Gen.choose(1, 5)
      (h :: tail) <- Gen.listOfN(pc, upperIdent)
    } yield PackageName(NonEmptyList(h, tail))

  val genConst: Gen[Type.Const] =
    Gen.zip(packageNameGen, typeNameGen)
      .map { case (p, n) => Type.Const.Defined(p, n) }

  val genBound: Gen[Type.Var.Bound] =
    lowerIdent.map { v => Type.Var.Bound(v) }

  def genRootType(genC: Option[Gen[Type.Const]]): Gen[Type] = {
    val b = genBound.map(Type.TyVar(_))
    genC match {
      case None => b
      case Some(gc) =>
        Gen.oneOf(gc.map(Type.TyConst(_)), b)
    }
  }

  def genDepth(d: Int, genC: Option[Gen[Type.Const]]): Gen[Type] =
    if (d <= 0) genRootType(genC)
    else {
      val recurse = Gen.lzy(genDepth(d - 1, genC))
      val genForAll =
        for {
          c <- Gen.choose(1, 5)
          as <- Gen.listOfN(c, genBound)
          in <- recurse
        } yield Type.forAll(as, in)

      val genApply = Gen.zip(recurse, recurse).map { case (a, b) => Type.TyApply(a, b) }

      Gen.oneOf(recurse, genApply, genForAll)
    }


  val genDepth03: Gen[Type] = Gen.choose(0, 3).flatMap(genDepth(_, Some(genConst)))
}
