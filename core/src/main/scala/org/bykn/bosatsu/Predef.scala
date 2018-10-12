package org.bykn.bosatsu

import cats.data.NonEmptyList
import fastparse.all._

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

  import Evaluation.Value
  import Value._

  private def i(a: Any): Int =
    VInt.unapply(a.asInstanceOf[Value]).get

  def add(a: Any, b: Any): Any =
    VInt(i(a) + i(b))

  def sub(a: Any, b: Any): Any =
    VInt(i(a) - i(b))

  def times(a: Any, b: Any): Any =
    VInt(i(a) * i(b))

  def eq_Int(a: Any, b: Any): Any =
    if (i(a) == i(b)) True else False

  def foldLeft(list: Any, bv: Any, fn: Any): Any = {
    val fnT = fn.asInstanceOf[FnValue]
    @annotation.tailrec
    def loop(list: Value, bv: Value): Value =
      list match {
        case SumValue(0, _) =>
          // Empty
          bv
        case SumValue(1, ConsValue(head, ConsValue(tail, UnitValue))) =>
          val nextBv = fnT(bv).flatMap(_.asFn(head)).value
          loop(tail, nextBv)
        case _ => sys.error(s"unexpected: $list")
      }

    loop(list.asInstanceOf[Value], bv.asInstanceOf[Value])
  }
}

