package org.bykn.bosatsu.codegen.python

import cats.data.NonEmptyList
import java.math.BigInteger
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.python.core.{ParserFacade => JythonParserFacade}
import org.scalatest.funsuite.AnyFunSuite

class CodeTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // PropertyCheckConfiguration(minSuccessful = 50000)
    // PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)

  lazy val genPy2Name: Gen[String] = {
    val letters = (('A' to 'Z') ++ ('a' to 'z')).toList
    val digits = ('0' to '9').toList
    for {
      c0 <- Gen.oneOf('_' :: letters)
      sz <- Gen.choose(0, 20)
      c1 <- Gen.listOfN(sz, Gen.oneOf('_' :: digits ::: letters))
      res0 = (c0 :: c1).mkString
      res <- if (Code.pyKeywordList(res0)) genPy2Name else Gen.const(res0)
    } yield res
  }

  val genIdent: Gen[Code.Ident] = genPy2Name.map(Code.Ident)

  def genExpr(depth: Int): Gen[Code.Expression] = {
    val genDotselect =
      genNel(5, genIdent)
        .map { nel =>
          nel.tail.foldLeft(nel.head: Code.Expression)(_.dot(_))
        }

    val genZero =
      Gen.oneOf(
        Gen.identifier.map(Code.PyString),
        genIdent,
        Gen.oneOf(
          Code.Const.Zero,
          Code.Const.One,
          Code.Const.True,
          Code.Const.False
        ),
        genDotselect,
        Gen.choose(-1024, 1024).map(Code.fromInt)
      )

    if (depth <= 0) genZero
    else {
      val rec = Gen.lzy(genExpr(depth - 1))

      val opName = Gen.oneOf(
        Code.Const.Minus,
        Code.Const.Plus,
        Code.Const.Times,
        Code.Const.Div,
        Code.Const.Mod,
        Code.Const.And,
        Code.Const.Eq,
        Code.Const.Neq,
        Code.Const.Gt,
        Code.Const.Lt
      )

      val genOp =
        Gen.zip(rec, opName, rec).map { case (a, b, c) => Code.Op(a, b, c) }

      val genTup =
        for {
          sz <- Gen.choose(0, 10)
          items <- Gen.listOfN(sz, rec)
        } yield Code.MakeTuple(items)

      val genList =
        for {
          sz <- Gen.choose(0, 10)
          items <- Gen.listOfN(sz, rec)
        } yield Code.MakeList(items)

      val genApp =
        for {
          fn <- rec
          sz <- Gen.choose(0, 10)
          items <- Gen.listOfN(sz, rec)
        } yield Code.Apply(fn, items)

      val genTern =
        Gen.zip(rec, rec, rec).map { case (t, c, f) => Code.Ternary(t, c, f) }

      Gen.frequency(
        (5, genZero),
        (1, genOp),
        (2, rec.map(Code.Parens(_))),
        (2, Gen.zip(rec, Gen.choose(0, 100)).map { case (a, p) => a.get(p) }),
        (
          1,
          Gen.zip(rec, Gen.option(rec), Gen.option(rec)).map { case (a, s, e) =>
            Code.SelectRange(a, s, e)
          }
        ),
        (1, Gen.oneOf(genTup, genList)), // these can really blow things up
        (
          2,
          Gen.zip(Gen.listOf(genIdent), rec).map { case (args, x) =>
            Code.Lambda(args, x)
          }
        ),
        (1, genApp),
        (1, genTern)
      )
    }
  }

  def genValueLike(depth: Int): Gen[Code.ValueLike] =
    if (depth <= 0) genExpr(0)
    else {
      val rec = Gen.lzy(genValueLike(depth - 1))
      val recX = Gen.lzy(genExpr(depth - 1))
      val recS = Gen.lzy(genStatement(depth - 1))

      val cond = Gen.zip(recX, rec)
      Gen.frequency(
        (10, recX),
        (1, Gen.zip(recS, rec).map { case (s, r) => s.withValue(r) }),
        (
          1,
          Gen.zip(genNel(4, cond), rec).map { case (conds, e) =>
            Code.IfElse(conds, e)
          }
        )
      )
    }

  def genNel[A](max: Int, genA: Gen[A]): Gen[NonEmptyList[A]] =
    for {
      cnt <- Gen.choose(1, max)
      lst <- Gen.listOfN(cnt, genA)
    } yield NonEmptyList.fromListUnsafe(lst)

  def genStatement(depth: Int): Gen[Code.Statement] = {
    val genZero = {
      val gp = Gen.const(Code.Pass)
      val genImp = Gen.zip(genPy2Name, Gen.option(genIdent)).map {
        case (m, a) => Code.Import(m, a)
      }

      Gen.oneOf(gp, genImp)
    }

    if (depth <= 0) genZero
    else {
      val recStmt = Gen.lzy(genStatement(depth - 1))
      val recExpr = Gen.lzy(genExpr(depth - 1))
      val recVL = Gen.lzy(genValueLike(depth - 1))

      val genCall =
        for {
          fn <- recExpr
          sz <- Gen.choose(0, 10)
          items <- Gen.listOfN(sz, recExpr)
        } yield Code.Call(Code.Apply(fn, items))

      val genClass =
        for {
          nm <- genIdent
          exCnt <- Gen.choose(0, 3)
          ex <- Gen.listOfN(exCnt, genIdent)
          body <- recStmt
        } yield Code.ClassDef(nm, ex, body)

      val genBlock = genNel(5, recStmt).map(Code.Block(_))
      val genRet = recVL.map(Code.toReturn(_))
      val genAlways = recVL.map(Code.always(_))
      val genAssign =
        Gen.zip(genIdent, recVL).map { case (v, e) => Code.addAssign(v, e) }
      val genWhile =
        Gen.zip(recExpr, recStmt).map { case (c, b) => Code.While(c, b) }
      val genIf =
        for {
          conds <- genNel(4, Gen.zip(recExpr, recStmt))
          elseCond <- Gen.option(recStmt)
        } yield Code.ifStatement(conds, elseCond)

      val genDef =
        for {
          name <- genIdent
          args <- Gen.listOf(genIdent)
          body <- recStmt
        } yield Code.Def(name, args, body)

      Gen.frequency(
        (20, genZero),
        (5, genWhile),
        (5, genAssign),
        (10, genRet),
        (1, genDef),
        (1, genBlock),
        (1, genIf),
        (1, genCall),
        (1, genClass),
        (1, genAlways)
      )
    }
  }

  def assertParse(str: String) = {
    try {
      val mod = JythonBarrier.run(
        JythonParserFacade.parseExpressionOrModule(
          new java.io.StringReader(str),
          "filename.py",
          new org.python.core.CompilerFlags()
        )
      )
      assert(mod != null)
    } catch {
      case _: Throwable =>
        val msg = "\n\n" + ("=" * 80) + "\n\n" + str + "\n\n" + ("=" * 80)
        assert(false, msg)
    }
  }

  def genCode(depth: Int): Gen[Code] =
    Gen.oneOf(genExpr(depth), genStatement(depth))

  test("All generated code can be parsed by jython") {
    forAll(genCode(4)) { code =>
      val str = Code.toDoc(code).render(100)

      assertParse(str)
    }
  }

  test("test bug with IfElse") {
    import Code._

    val ifElse = IfElse(NonEmptyList.of((Ident("a"), Ident("b"))), Ident("c"))
    val stmt = addAssign(Ident("bar"), ifElse)

    assert(toDoc(stmt).renderTrim(80) == """if a:
    bar = b
else:
    bar = c""")
  }

  test("test some Operator examples") {
    import Code._

    val apbpc =
      Op(Ident("a"), Const.Plus, Op(Ident("b"), Const.Plus, Ident("c")))

    assert(toDoc(apbpc).renderTrim(80) == """a + b + c""")

    val apbmc =
      Op(Ident("a"), Const.Plus, Op(Ident("b"), Const.Minus, Ident("c")))

    assert(toDoc(apbmc).renderTrim(80) == """a + b - c""")

    val ambmc =
      Op(Ident("a"), Const.Minus, Op(Ident("b"), Const.Minus, Ident("c")))

    assert(toDoc(ambmc).renderTrim(80) == """a - (b - c)""")

    val amzmbmc = Op(
      Op(Ident("a"), Const.Minus, Ident("z")),
      Const.Minus,
      Op(Ident("b"), Const.Minus, Ident("c"))
    )

    assert(toDoc(amzmbmc).renderTrim(80) == """(a - z) - (b - c)""")
  }

  test("x.eval(Eq, x) == True") {
    forAll(genExpr(4)) { x =>
      assert(x.eval(Code.Const.Eq, x) == Code.Const.True)
    }
  }
  test("we can do integer comparisons") {
    forAll { (i1: Int, i2: Int) =>
      val cmp = i1.compareTo(i2)

      val p1 = Code.fromInt(i1)
      val p2 = Code.fromInt(i2)

      if (cmp == 0) {
        assert(p1.eval(Code.Const.Eq, p2) == Code.Const.True)
      } else if (cmp < 0) {
        assert(p1.eval(Code.Const.Lt, p2) == Code.Const.True)
      } else {
        assert(p1.eval(Code.Const.Gt, p2) == Code.Const.True)
      }
    }
  }

  test("x.evalAnd(True) == x") {
    forAll(genExpr(4)) { x =>
      assert(x.evalAnd(Code.Const.True) == x.simplify)
      assert(Code.Const.True.evalAnd(x) == x.simplify)
    }
  }
  test("x.evalAnd(False) == False") {
    forAll(genExpr(4)) { x =>
      assert(x.evalAnd(Code.Const.False) == Code.Const.False)
      assert(Code.Const.False.evalAnd(x) == Code.Const.False)
    }
  }

  test("x.evalPlus(y) == (x + y)") {
    forAll { (x: Int, y: Int) =>
      val cx = Code.fromInt(x)
      val cy = Code.fromInt(y)
      assert(cx.evalPlus(cy) == Code.fromLong(x.toLong + y.toLong))
    }
  }

  test("x.evalMinus(y) == (x - y)") {
    forAll { (x: Int, y: Int) =>
      val cx = Code.fromInt(x)
      val cy = Code.fromInt(y)
      assert(cx.evalMinus(cy) == Code.fromLong(x.toLong - y.toLong))
    }
  }

  test("x.evalTimes(y) == (x * y)") {
    forAll { (x: Int, y: Int) =>
      val cx = Code.fromInt(x)
      val cy = Code.fromInt(y)
      assert(cx.evalTimes(cy) == Code.fromLong(x.toLong * y.toLong))
    }
  }

  test("x.eval(op, y).eval(op, z) == op(op(x, y), z") {
    val gi = Gen.choose(-1024L, 1024L)

    val gop = Gen.oneOf(Code.Const.Plus, Code.Const.Minus, Code.Const.Times)
    forAll(gi, gi, gi, gop, gop) { (a, b, c, op1, op2) =>
      val left = Code.Op(
        Code.Op(Code.fromLong(a), op1, Code.fromLong(b)),
        op2,
        Code.fromLong(c)
      )
      assert(
        left.simplify == Code.PyInt(
          op2(
            op1(BigInteger.valueOf(a), BigInteger.valueOf(b)),
            BigInteger.valueOf(c)
          )
        )
      )

      val right = Code.Op(
        Code.fromLong(a),
        op1,
        Code.Op(Code.fromLong(b), op2, Code.fromLong(c))
      )
      assert(
        right.simplify == Code.PyInt(
          op1(
            BigInteger.valueOf(a),
            op2(BigInteger.valueOf(b), BigInteger.valueOf(c))
          )
        )
      )
    }
  }

  def runAll(op: Code.Expression): Option[Code.PyInt] =
    op match {
      case pi @ Code.PyInt(_) => Some(pi)
      case Code.Op(left, op: Code.IntOp, right) =>
        for {
          l <- runAll(left)
          r <- runAll(right)
        } yield Code.PyInt(op(l.toBigInteger, r.toBigInteger))
      case _ => None
    }

  def genOp(
      depth: Int,
      go: Gen[Code.IntOp],
      gen0: Gen[Code.Expression]
  ): Gen[Code.Expression] =
    if (depth <= 0) gen0
    else {
      val rec = Gen.lzy(genIntOp(depth - 1, go))
      Gen.oneOf(
        rec,
        Gen.zip(rec, go, rec).map { case (a, op, b) => Code.Op(a, op, b) }
      )
    }

  def genIntOp(depth: Int, go: Gen[Code.IntOp]): Gen[Code.Expression] =
    genOp(depth, go, Gen.choose(-1024, 1024).map(Code.fromInt))

  test("any sequence of IntOps is optimized") {
    forAll(
      genIntOp(
        5,
        Gen.oneOf(Code.Const.Plus, Code.Const.Minus, Code.Const.Times)
      )
    ) { op =>
      // adding zero collapses to an Int
      assert(Some(op.evalPlus(Code.fromInt(0))) == runAll(op))
      assert(Some(Code.fromInt(0).evalPlus(op)) == runAll(op))
      assert(Some(op.evalMinus(Code.fromInt(0))) == runAll(op))
      assert(
        Some(Code.fromInt(0).evalMinus(op)) == runAll(
          op.evalTimes(Code.fromInt(-1))
        )
      )
      assert(Some(Code.fromInt(1).evalTimes(op)) == runAll(op))
    }
  }

  test("any sequence of +/- leaves only a single PyInt at the end") {
    val gen = genOp(
      5,
      Gen.oneOf(Code.Const.Plus, Code.Const.Minus),
      Gen.oneOf(
        Gen.choose(-1024, 1024).map(Code.fromInt),
        Gen.identifier.map(Code.Ident(_))
      )
    )

    forAll(gen) { op =>
      val simpOp = op.simplify
      def assertGood(
          x: Code.Expression,
          isRight: Boolean
      ): org.scalatest.Assertion =
        x match {
          case Code.PyInt(_) =>
            assert(isRight, s"found: $x on the left inside of $simpOp")
          case Code.Op(left, _, right) =>
            assertGood(left, false)
            assertGood(right, isRight)
          case _ =>
            // not an int or op, this is fine
            assert(true)
        }

      assertGood(simpOp, true)
    }
  }

  test("Code.block as at most 1 Pass and if so, only at the end") {
    import Code._

    assert(block(Pass) == Pass)
    assert(block(Pass, Pass) == Pass)

    forAll(genNel(4, genStatement(3))) { case NonEmptyList(h, t) =>
      val stmt = block(h, t: _*)

      def passCount(s: Statement): Int =
        s match {
          case Pass     => 1
          case Block(s) => s.toList.map(passCount).sum
          case _        => 0
        }

      def notPassCount(s: Statement): Int =
        s match {
          case Pass     => 0
          case Block(s) => s.toList.map(notPassCount).sum
          case _        => 1
        }

      val pc = passCount(stmt)
      assert(pc <= 1)
      if (pc == 1) {
        t.foreach { s =>
          assert(notPassCount(s) == 0)
        }
        assert(notPassCount(h) == 0)
      }
    }
  }

  test("simplify is idempotent") {
    forAll(genExpr(4)) { expr =>
      assert(expr.simplify.simplify == expr.simplify)
    }

    val regressions: List[Code.Expression] =
      List(
        Code.SelectItem(
          Code.Ternary(
            Code.fromInt(0),
            Code.fromInt(0),
            Code.MakeTuple(List(Code.fromInt(42)))
          ),
          0
        )
      )

    regressions.foreach { expr =>
      assert(expr.simplify.simplify == expr.simplify)
    }
  }

  test("simplify on Ternary removes branches when possible") {
    forAll(genExpr(3), genExpr(3), genExpr(3)) { (t, c, f) =>
      val tern = Code.Ternary(t, c, f).simplify
      c.simplify match {
        case Code.PyBool(b) =>
          if (b) {
            assert(tern == t.simplify)
          } else {
            assert(tern == f.simplify)
          }
        case Code.PyInt(i) =>
          if (i != BigInteger.ZERO) {
            assert(tern == t.simplify)
          } else {
            assert(tern == f.simplify)
          }
        case whoKnows =>
          assert(tern == Code.Ternary(t.simplify, whoKnows, f.simplify))
      }
    }
  }

  test("identOrParens is true") {
    forAll(genExpr(4)) { expr =>
      expr.identOrParens match {
        case Code.Ident(_) | Code.Parens(_) => assert(true)
        case other                          => assert(false, other.toString)
      }
    }
  }

  test("(a == b) and (b == c) renders correctly") {
    val left = Code.Op(Code.Ident("a"), Code.Const.Eq, Code.Ident("b"))
    val right = Code.Op(Code.Ident("b"), Code.Const.Eq, Code.Ident("c"))
    val and = left.evalAnd(right)

    assert(Code.toDoc(and).renderTrim(80) == "(a == b) and (b == c)")
    assert(
      Code
        .toDoc(Code.Ident("z").evalAnd(and))
        .renderTrim(80) == "z and (a == b) and (b == c)"
    )
  }
}
