package org.bykn.bosatsu

import org.scalatest.FunSuite

import rankn._

import Parser.Combinators
import fastparse.all.Parsed

class TotalityTest extends FunSuite {
  import TestParseUtils._

  val pack = PackageName.parts("Test")
  def const(t: String): Type =
    Type.TyConst(Type.Const.Defined(pack, t))

  def typeEnvOf(str: String): TypeEnv =
    Statement.parser.parse(str) match {
      case Parsed.Success(stmt, idx) =>
        assert(idx == str.length)
        val prog = Program.fromStatement(
          pack,
          { tpe => Type.Const.Defined(pack, tpe) },
          { cons => (pack, ConstructorName(cons)) },
          stmt)
        prog.types
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
        sys.error("could not produce TypeEnv")
    }

  def patterns(str: String): List[Pattern[(PackageName, ConstructorName), Type]] =
    Pattern.parser.listSyntax.parse(str) match {
      case Parsed.Success(pats, idx) =>
        pats.map { pat =>
          pat
            .mapName { n => (pack, ConstructorName(n)) }
            .mapType(_.toType { n => Type.Const.Defined(pack, n) })
        }
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
        sys.error("could not produce TypeEnv")
    }

  def notTotal(te: TypeEnv, pats: List[Pattern[(PackageName, ConstructorName), Type]]) =
    TotalityCheck(te).isTotal(pats) match {
      case Right(res) => assert(!res, pats.toString)
      case Left(errs) => fail(errs.toString)
    }

  def testTotality(te: TypeEnv, pats: List[Pattern[(PackageName, ConstructorName), Type]], tight: Boolean = false) = {
    TotalityCheck(te).isTotal(pats) match {
      case Right(res) => assert(res)
      case Left(errs) => fail(errs.toString)
    }
    // any missing pattern shouldn't be total:
    def allButOne[A](head: A, tail: List[A]): List[List[A]] =
      tail match {
        case Nil => Nil
        case h :: rest =>
          // we can either delete the head or one from the tail:
          val keepHead = allButOne(h, rest).map(head :: _)
          tail :: keepHead
      }

    pats match {
      case h :: tail if tight =>
        allButOne(h, tail).foreach(notTotal(te, _))
      case _ => ()
    }
  }



  test("totality test") {
    val te = typeEnvOf("""#
struct Unit
""")
    val pats = patterns("[Unit]")
    testTotality(te, pats)


    val te1 = typeEnvOf("""#
struct Tuple2(a, b)
""")
    testTotality(te1, patterns("[Tuple2(_, _)]"))
    testTotality(te1, patterns("[Tuple2(_, 0), Tuple2(_, _)]"))
    notTotal(te1, patterns("[Tuple2(_, 0)]"))
  }

  test("test Option types") {
    val te = typeEnvOf("""#
enum Option: None, Some(get)
""")
    testTotality(te, patterns("[Some(_), None]"), tight = true)
    testTotality(te, patterns("[Some(_), _]"))
    testTotality(te, patterns("[Some(1), Some(x), None]"))
    testTotality(te, patterns("[Some(Some(_)), Some(None), None]"), tight = true)

    notTotal(te, patterns("[Some(_)]"))
    notTotal(te, patterns("[Some(Some(_)), None]"))
    notTotal(te, patterns("[None]"))
    notTotal(te, patterns("[]"))
  }

  test("test Either types") {
    val te = typeEnvOf("""#
enum Either: Left(l), Right(r)
""")
    testTotality(te, patterns("[Left(_), Right(_)]"))
    testTotality(te,
      patterns("[Left(Right(_)), Left(Left(_)), Right(Left(_)), Right(Right(_))]"),
      tight = true)

    notTotal(te, patterns("[Left(_)]"))
    notTotal(te, patterns("[Right(_)]"))
    notTotal(te, patterns("[Left(Right(_)), Right(_)]"))
  }

  test("test List matching") {
    testTotality(TypeEnv.empty, patterns("[[], [h, *tail]]"), tight = true)
    testTotality(TypeEnv.empty, patterns("[[], [h, *tail], [h0, h1, *tail]]"), tight = true)

    notTotal(TypeEnv.empty, patterns("[[], [h, *tail, _]]"))
    notTotal(TypeEnv.empty, patterns("[[], [*tail, _]]"))
  }

  test("multiple struct compose") {
    val te = typeEnvOf("""#
enum Either: Left(l), Right(r)
enum Option: None, Some(get)
""")

    testTotality(te, patterns("[None, Some(Left(_)), Some(Right(_))]"), tight = true)
  }

  test("compose List with structs") {
    val te = typeEnvOf("""#
enum Either: Left(l), Right(r)
""")
    testTotality(te, patterns("[[Left(_), *_], [Right(_), *_], [], [_, _, *_]]"), tight = true)
  }
}
