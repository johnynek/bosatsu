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
        "foldLeft",
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
      .add(predefPackage.name, "foldLeft", FfiCall.ScalaCall("org.bykn.bosatsu.PredefImpl.foldLeft"))

  def withPredef(ps: List[Package.Parsed]): List[Package.Parsed] =
    predefPackage :: ps.map(_.withImport(predefImports))

  def withPredefA[A](predefA: A, ps: List[(A, Package.Parsed)]): List[(A, Package.Parsed)] =
    (predefA, predefPackage) :: ps.map { case (a, p) => (a, p.withImport(predefImports)) }
}

object PredefImpl {
  val True: AnyRef = (1, Nil)
  val False: AnyRef = (0, Nil)

  private def i(a: Any): JInteger = a.asInstanceOf[JInteger]

  def add(a: Any, b: Any): Any =
    JInteger.valueOf(i(a).intValue + i(b).intValue)

  def sub(a: Any, b: Any): Any =
    JInteger.valueOf(i(a).intValue - i(b).intValue)

  def times(a: Any, b: Any): Any =
    JInteger.valueOf(i(a).intValue * i(b).intValue)

  def eq_Int(a: Any, b: Any): Any =
    if (i(a).intValue == i(b).intValue) True else False

  def foldLeft(list: Any, bv: Any, fn: Any): Any = {
    val fnT = fn.asInstanceOf[Fn[Any, Fn[Any, Any]]]
    @annotation.tailrec
    def loop(list: Any, bv: Any): Any =
      list match {
        case (0, _) =>
          // Empty
          bv
        case (1, head :: tail :: Nil) =>
          val nextBv = fnT(bv)(head)
          loop(tail, nextBv)
        case _ => sys.error(s"unexpected: $list")
      }

    loop(list, bv)
  }
}

