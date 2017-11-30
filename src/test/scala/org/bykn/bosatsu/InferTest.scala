package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.scalatest._
import fastparse.all._

class InferTest extends FunSuite {
  implicit val unitRegion: HasRegion[Unit] = HasRegion.instance[Unit](_ => Region(0, 0))

  def simpleMatch[T: HasRegion](e: Expr[T], t: Type) = {
    assert(Inference.inferExpr(e).map(_.tag._2) === Right(Scheme(Nil, t)))
  }
  val i1 = Expr.Literal(Lit.Integer(1), ())

  val testPack = PackageName(NonEmptyList("Test", Nil))

  test("int") {
    simpleMatch(i1, Type.intT)
    simpleMatch(Expr.Op(i1, Operator.Plus, i1, ()), Type.intT)
  }

  test("bool") {
    simpleMatch(Expr.Op(i1, Operator.Eql, i1, ()), Type.boolT)
  }

  def parseType(str: String, t: Type) =
    Declaration.parser("").parse(str) match {
      case Parsed.Success(decl, _) =>
        val expr = decl.toExpr(testPack)
        Inference.inferExpr(TypeEnv.empty(PackageName(NonEmptyList.of("InferTest", "ParseType"))), expr) match {
          case Left(f) => fail(s"failed: $f")
          case Right(s) => assert(s.tag._2.result === t, s"$str => $decl => $expr => $s")
        }
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
    }

  def parseProgram(str: String, t: Type) =
    Statement.parser.parse(str) match {
      case Parsed.Success(exp, _) =>
        val prog = exp.toProgram(testPack)
        prog.getMainDecl match {
          case None => fail(s"found no main expression")
          case Some(main) =>
            Inference.inferExpr(prog.types, main) match {
              case Left(f) => fail(s"failed: $f")
              case Right(s) => assert(s.tag._2.result == t, s"$str => $exp => $main => $s")
            }
        }
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
    }

  test("type check some expressions") {
    parseType("1 + 1", Type.intT)
    parseType("1 == 1", Type.boolT)
    parseType("(1+2) == 1", Type.boolT)
    parseType("""(\x -> x + 1)(2)""", Type.intT)
    parseType("""(\x -> x + 1)""", Type.Arrow(Type.intT, Type.intT))
    parseType(
"""x = 1
y = x
y""", Type.intT)

    parseType(
"""fn = \x, y -> x + y
fn""", Type.Arrow(Type.intT, Type.Arrow(Type.intT, Type.intT)))

    parseType(
"""\x ->
  fn = \y -> x + y
  fn""", Type.Arrow(Type.intT, Type.Arrow(Type.intT, Type.intT)))
  }

  test("test inference with some defined types") {
    parseProgram("""#
struct Unit

main = Unit
""", Type.Declared(testPack, "Unit"))

    parseProgram("""#
enum Option:
  None
  Some(a)

main = Some(1)
""", Type.TypeApply(Type.Declared(testPack, "Option"), Type.intT))

    parseProgram("""#
enum Option:
  None
  Some(a)

main = Some
""", Type.Arrow(Type.Var("a"), Type.TypeApply(Type.Declared(testPack, "Option"), Type.Var("a"))))

   parseProgram("""#
enum Option:
  None
  Some(a)

x = Some(1)
main = match x:
  None:
    0
  Some(y):
    y
""", Type.intT)

   parseProgram("""#
enum List:
  Empty
  NonEmpty(a: a, tail: List[a])

x = NonEmpty(1, Empty)
main = match x:
  Empty:
    0
  NonEmpty(y, z):
    y + 1
""", Type.intT)
}

  // def evalTest(str: String, v: Any) =
  //   Parser.expr.parse(str) match {
  //     case Parsed.Success(exp, _) =>
  //       assert(Expr.evaluate(exp).right.get._1 == v)
  //     case Parsed.Failure(exp, idx, extra) =>
  //       fail(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
  //   }

  // test("evaluation works") {
  //   evalTest("1 + 1", 2)
  //   evalTest("(2 + 4) == 6", true)
  //   evalTest("""(lambda x: x)(2)""", 2)
  //   evalTest("(\\x: x)(2)", 2)
  //   evalTest("""x = 2
// y = lambda z:
  // x * z
// y(100)""", 200)
  // }

}
