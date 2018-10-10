package org.bykn.bosatsu

import cats.data.NonEmptyList
import fastparse.all._
import java.lang.{Integer => JInteger}

object Predef {
  private def resourceToString(path: String): Option[String] = {
    Option(getClass().getResourceAsStream(path)).map { stream =>
      scala.io.Source.fromInputStream(stream)("UTF-8").mkString
    }
  }

  private val predefString: String =
    resourceToString("/bosatsu/predef.bosatsu")
      .getOrElse {
        scala.io.Source.fromFile("target/scala-2.12/classes/bosatsu/predef.bosatsu").mkString
      }

  val predefPackage: Package.Parsed =
    Package.parser.parse(predefString) match {
      case Parsed.Success(pack, _) => pack
      case Parsed.Failure(_, _, _) => sys.error(s"could not parse $predefString")
    }

  def packageName: PackageName =
    PackageName.predef

  val predefImports: Import[PackageName, Unit] =
    Import(packageName,
      NonEmptyList.of(
        "Bool",
        "EmptyList",
        "False",
        "Int",
        "List",
        "NonEmptyList",
        "None",
        "Option",
        "Some",
        "String",
        "True",
        "Test",
        "TestAssert",
        "TestLabel",
        "TestList",
        "add",
        "eq_Int",
        "sub",
        "times"
        )
        .map(ImportedName.OriginalName(_, ())))

  val jvmExternals: Externals =
    Externals
      .empty
      .add(predefPackage.name, "add", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.add"))
      .add(predefPackage.name, "sub", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.sub"))
      .add(predefPackage.name, "times", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.times"))
      .add(predefPackage.name, "eq_Int", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.eq_Int"))

  def withPredef(ps: List[Package.Parsed]): List[Package.Parsed] =
    predefPackage :: ps.map(_.withImport(predefImports))

  def withPredefA[A](predefA: A, ps: List[(A, Package.Parsed)]): List[(A, Package.Parsed)] =
    (predefA, predefPackage) :: ps.map { case (a, p) => (a, p.withImport(predefImports)) }
}

object PredefImpl {
  val True: AnyRef = (1, Nil)
  val False: AnyRef = (0, Nil)

  def add(a: JInteger, b: JInteger): JInteger =
    JInteger.valueOf(a.intValue + b.intValue)

  def sub(a: JInteger, b: JInteger): JInteger =
    JInteger.valueOf(a.intValue - b.intValue)

  def times(a: JInteger, b: JInteger): JInteger =
    JInteger.valueOf(a.intValue * b.intValue)

  def eq_Int(a: JInteger, b: JInteger): AnyRef =
    if (a.intValue == b.intValue) True else False
}

