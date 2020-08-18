package org.bykn.bosatsu.codegen.python

import cats.data.NonEmptyList
import java.math.BigInteger
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite
import org.python.core.{ParserFacade => JythonParserFacade}

class CodeTest extends FunSuite {
  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 500)

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
          nel.tail.foldLeft(nel.head: Code.Dotable)(Code.DotSelect(_, _))
        }

    val genZero =
      Gen.oneOf(
        Gen.identifier.map(Code.PyString),
        genIdent,
        Gen.oneOf(Code.Const.Zero, Code.Const.One, Code.Const.True, Code.Const.False),
        genDotselect,
        Gen.choose(-1024, 1024).map(Code.fromInt))


    if (depth <= 0) genZero
    else {
      val rec = Gen.lzy(genExpr(depth - 1))

      val opName = Gen.oneOf(Code.Const.Minus, Code.Const.Plus, Code.Const.And, Code.Const.Eq, Code.Const.Gt)
      val genOp = Gen.zip(rec, opName, rec).map { case (a, b, c) => Code.Op(a, b, c) }

      val genTup =
        for {
          sz <- Gen.choose(0, 10)
          items <- Gen.listOfN(sz, rec)
        } yield Code.MakeTuple(items)

      val genApp =
        for {
          fn <- rec
          sz <- Gen.choose(0, 10)
          items <- Gen.listOfN(sz, rec)
        } yield Code.Apply(fn, items)

      Gen.frequency(
        (5, genZero),
        (1, genOp),
        (2, rec.map(Code.Parens(_))),
        (2, Gen.zip(rec, Gen.choose(0, 100)).map { case (a, p) => Code.SelectItem(a, p) }),
        (1, genTup),
        (2, Gen.zip(Gen.listOf(genIdent), rec).map { case (args, x) => Code.Lambda(args, x) }),
        (1, genApp)
      )
    }
  }

  def genNel[A](max: Int, genA: Gen[A]): Gen[NonEmptyList[A]] =
    for {
      cnt <- Gen.choose(1, max)
      lst <- Gen.listOfN(cnt, genA)
    } yield NonEmptyList.fromListUnsafe(lst)

  def genStatement(depth: Int): Gen[Code.Statement] = {
    val genZero = {
      val gp = Gen.const(Code.Pass)
      val genImp = Gen.zip(genPy2Name, Gen.option(genIdent)).map { case (m, a) => Code.Import(m, a) }

      Gen.oneOf(gp, genImp)
    }

    if (depth <= 0) genZero
    else {
      val recStmt = Gen.lzy(genStatement(depth - 1))
      val recExpr = Gen.lzy(genExpr(depth - 1))

      val genBlock = genNel(5, recStmt).map(Code.Block(_))
      val genRet = recExpr.map(Code.Return(_))
      val genAssign = Gen.zip(genIdent, recExpr).map { case (v, e) => Code.Assign(v, e) }
      val genWhile = Gen.zip(recExpr, recStmt).map { case (c, b) => Code.While(c, b) }
      val genIf =
        for {
          conds <- genNel(4, Gen.zip(recExpr, recStmt))
          elseCond <- Gen.option(recStmt)
        } yield Code.IfStatement(conds, elseCond)

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
        (1, genBlock),
        (1, genIf)
      )
    }
  }

  def assertParse(str: String) = {
    try {
      val mod = JythonParserFacade.parseExpressionOrModule(new java.io.StringReader(str), "filename.py", new org.python.core.CompilerFlags())
      assert(mod != null)
    }
    catch {
      case x: Throwable =>
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

    val ifElse = IfElse(NonEmptyList.of((Literal("a"), Literal("b"))), Literal("c"))
    val stmt = addAssign(Ident("bar"), ifElse)

    assert(toDoc(stmt).renderTrim(80) == """if a:
    bar = b
else:
    bar = c""")
  }

  test("test some Operator examples") {
    import Code._

    val apbpc = Op(Literal("a"), Const.Plus, Op(Literal("b"), Const.Plus, Literal("c")))

    assert(toDoc(apbpc).renderTrim(80) == """a + b + c""")

    val apbmc = Op(Literal("a"), Const.Plus, Op(Literal("b"), Const.Minus, Literal("c")))

    assert(toDoc(apbmc).renderTrim(80) == """a + b - c""")

    val ambmc = Op(Literal("a"), Const.Minus, Op(Literal("b"), Const.Minus, Literal("c")))

    assert(toDoc(ambmc).renderTrim(80) == """a - (b - c)""")

    val amzmbmc = Op(Op(Literal("a"), Const.Minus, Literal("z")), Const.Minus, Op(Literal("b"), Const.Minus, Literal("c")))

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
      }
      else if (cmp < 0) {
        assert(p1.eval(Code.Const.Lt, p2) == Code.Const.True)
      }
      else {
        assert(p1.eval(Code.Const.Gt, p2) == Code.Const.True)
      }
    }
  }

  test("x.evalAnd(True) == x") {
    forAll(genExpr(4)) { x =>
      assert(x.evalAnd(Code.Const.True) == x)
      assert(Code.Const.True.evalAnd(x) == x)
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
      val left = Code.Op(Code.Op(Code.fromLong(a), op1, Code.fromLong(b)), op2, Code.fromLong(c))
      assert(left.simplify == Code.PyInt(op2(op1(BigInteger.valueOf(a), BigInteger.valueOf(b)), BigInteger.valueOf(c))))

      val right = Code.Op(Code.fromLong(a), op1, Code.Op(Code.fromLong(b), op2, Code.fromLong(c)))
      assert(right.simplify == Code.PyInt(op1(BigInteger.valueOf(a), op2(BigInteger.valueOf(b), BigInteger.valueOf(c)))))
    }
  }

  def runAll(op: Code.Expression): Option[Code.PyInt] =
    op match {
      case pi@Code.PyInt(_) => Some(pi)
      case Code.Op(left, op: Code.IntOp, right) =>
        for {
          l <- runAll(left)
          r <- runAll(right)
        } yield Code.PyInt(op(l.toBigInteger, r.toBigInteger))
      case _ => None
    }

  def genOp(depth: Int, go: Gen[Code.IntOp], gen0: Gen[Code.Expression]): Gen[Code.Expression] =
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
    forAll(genIntOp(5, Gen.oneOf(Code.Const.Plus, Code.Const.Minus, Code.Const.Times))) { op =>
      // adding zero collapses to an Int
      assert(Some(op.evalPlus(Code.fromInt(0))) == runAll(op))
      assert(Some(Code.fromInt(0).evalPlus(op)) == runAll(op))
      assert(Some(op.evalMinus(Code.fromInt(0))) == runAll(op))
      assert(Some(Code.fromInt(0).evalMinus(op)) == runAll(op.evalTimes(Code.fromInt(-1))))
      assert(Some(Code.fromInt(1).evalTimes(op)) == runAll(op))
    }
  }

  test("any sequence of +/- leaves only a single PyInt at the end") {
    val gen = genOp(
      5,
      Gen.oneOf(Code.Const.Plus, Code.Const.Minus),
      Gen.oneOf(Gen.choose(-1024, 1024).map(Code.fromInt), Gen.identifier.map(Code.Ident(_))))

    forAll(gen) { op =>
      val simpOp = op.simplify
      def assertGood(x: Code.Expression, isRight: Boolean): org.scalatest.Assertion =
        x match {
          case Code.PyInt(_) => assert(isRight, s"found: $x on the left inside of $simpOp")
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
}
