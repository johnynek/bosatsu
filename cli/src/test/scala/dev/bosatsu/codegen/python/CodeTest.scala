package dev.bosatsu.codegen.python

import cats.data.NonEmptyList
import java.math.BigInteger
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.python.core.{ParserFacade => JythonParserFacade}

class CodeTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    // PropertyCheckConfiguration(minSuccessful = 50000)
    // PropertyCheckConfiguration(minSuccessful = 5000)
    super.scalaCheckTestParameters.withMinSuccessfulTests(1000)

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

  val genIdent: Gen[Code.Ident] = genPy2Name.map(Code.Ident(_))

  def genExpr(depth: Int): Gen[Code.Expression] = {
    val genDotselect =
      genNel(5, genIdent)
        .map { nel =>
          nel.tail.foldLeft(nel.head: Code.Expression)(_.dot(_))
        }

    val genZero =
      Gen.oneOf(
        Gen.identifier.map(Code.PyString(_)),
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

  def assertParse(str: String) =
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
      case t: Throwable =>
        val msg = "\n\n" + ("=" * 80) + "\n\n" + str + "\n\n" + ("=" * 80)
        val err = s"\n\n${t.getClass.getName}: ${t.getMessage}"
        assert(false, msg + err)
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

    assertEquals(toDoc(stmt).renderTrim(80), """if a:
    bar = b
else:
    bar = c""")
  }

  test("test some Operator examples") {
    import Code._

    val apbpc =
      Op(Ident("a"), Const.Plus, Op(Ident("b"), Const.Plus, Ident("c")))

    assertEquals(toDoc(apbpc).renderTrim(80), """a + b + c""")

    val apbmc =
      Op(Ident("a"), Const.Plus, Op(Ident("b"), Const.Minus, Ident("c")))

    assertEquals(toDoc(apbmc).renderTrim(80), """a + b - c""")

    val ambmc =
      Op(Ident("a"), Const.Minus, Op(Ident("b"), Const.Minus, Ident("c")))

    assertEquals(toDoc(ambmc).renderTrim(80), """a - (b - c)""")

    val amzmbmc = Op(
      Op(Ident("a"), Const.Minus, Ident("z")),
      Const.Minus,
      Op(Ident("b"), Const.Minus, Ident("c"))
    )

    assertEquals(toDoc(amzmbmc).renderTrim(80), """(a - z) - (b - c)""")
  }

  test("x.eval(Eq, x) == True") {
    forAll(genExpr(4)) { x =>
      assertEquals(x.eval(Code.Const.Eq, x), Code.Const.True)
    }
  }
  test("we can do integer comparisons") {
    forAll { (i1: Int, i2: Int) =>
      val cmp = i1.compareTo(i2)

      val p1 = Code.fromInt(i1)
      val p2 = Code.fromInt(i2)

      if (cmp == 0) {
        assertEquals(p1.eval(Code.Const.Eq, p2), Code.Const.True)
      } else if (cmp < 0) {
        assertEquals(p1.eval(Code.Const.Lt, p2), Code.Const.True)
      } else {
        assertEquals(p1.eval(Code.Const.Gt, p2), Code.Const.True)
      }
    }
  }

  test("x.evalAnd(True) == x") {
    forAll(genExpr(4)) { x =>
      assertEquals(x.evalAnd(Code.Const.True), x.simplify)
      assertEquals(Code.Const.True.evalAnd(x), x.simplify)
    }
  }
  test("x.evalAnd(False) == False") {
    forAll(genExpr(4)) { x =>
      val sx = x.simplify
      val res = x.evalAnd(Code.Const.False)
      assert(
        (res == Code.Const.False) || ((res == sx) && (sx == Code.Const.Zero))
      )
      assertEquals(Code.Const.False.evalAnd(x), Code.Const.False)
    }
  }

  test("x.evalPlus(y) == (x + y)") {
    forAll { (x: Int, y: Int) =>
      val cx = Code.fromInt(x)
      val cy = Code.fromInt(y)
      assertEquals(cx.evalPlus(cy), Code.fromLong(x.toLong + y.toLong))
    }
  }

  test("x.evalMinus(y) == (x - y)") {
    forAll { (x: Int, y: Int) =>
      val cx = Code.fromInt(x)
      val cy = Code.fromInt(y)
      assertEquals(cx.evalMinus(cy), Code.fromLong(x.toLong - y.toLong))
    }
  }

  test("x.evalTimes(y) == (x * y)") {
    forAll { (x: Int, y: Int) =>
      val cx = Code.fromInt(x)
      val cy = Code.fromInt(y)
      assertEquals(cx.evalTimes(cy), Code.fromLong(x.toLong * y.toLong))
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
      assertEquals(left.simplify, Code.PyInt(
          op2(
            op1(BigInteger.valueOf(a), BigInteger.valueOf(b)),
            BigInteger.valueOf(c)
          )
        ))

      val right = Code.Op(
        Code.fromLong(a),
        op1,
        Code.Op(Code.fromLong(b), op2, Code.fromLong(c))
      )
      assertEquals(right.simplify, Code.PyInt(
          op1(
            BigInteger.valueOf(a),
            op2(BigInteger.valueOf(b), BigInteger.valueOf(c))
          )
        ))
    }
  }

  def runAll(op: Code.Expression): Option[Code.PyInt] =
    op match {
      case pi @ Code.PyInt(_)                   => Some(pi)
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
    def asPyInt(expr: Code.Expression): Option[Code.PyInt] =
      expr match {
        case pi: Code.PyInt => Some(pi)
        case _              => None
      }

    forAll(
      genIntOp(
        5,
        Gen.oneOf(Code.Const.Plus, Code.Const.Minus, Code.Const.Times)
      )
    ) { op =>
      // adding zero collapses to an Int
      assertEquals(asPyInt(op.evalPlus(Code.fromInt(0))), runAll(op))
      assertEquals(asPyInt(Code.fromInt(0).evalPlus(op)), runAll(op))
      assertEquals(asPyInt(op.evalMinus(Code.fromInt(0))), runAll(op))
      assertEquals(asPyInt(Code.fromInt(0).evalMinus(op)), runAll(
          op.evalTimes(Code.fromInt(-1))
        ))
      assertEquals(asPyInt(Code.fromInt(1).evalTimes(op)), runAll(op))
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
      ): Unit =
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

    assertEquals(block(Pass), Pass)
    assertEquals(block(Pass, Pass), Pass)

    forAll(genNel(4, genStatement(3))) { case NonEmptyList(h, t) =>
      val stmt = block(h, t*)

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
          assertEquals(notPassCount(s), 0)
        }
        assertEquals(notPassCount(h), 0)
      }
    }
  }

  test("simplify is idempotent") {
    val prop = forAll(genExpr(4)) { expr =>
      assertEquals(expr.simplify.simplify, expr.simplify)
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
      assertEquals(expr.simplify.simplify, expr.simplify)
    }

    prop
  }

  test("simplify on Ternary removes branches when possible") {
    forAll(genExpr(3), genExpr(3), genExpr(3)) { (t, c, f) =>
      val tern = Code.Ternary(t, c, f).simplify
      c.simplify match {
        case Code.PyBool(b) =>
          if (b) {
            assertEquals(tern, t.simplify)
          } else {
            assertEquals(tern, f.simplify)
          }
        case Code.PyInt(i) =>
          if (i != BigInteger.ZERO) {
            assertEquals(tern, t.simplify)
          } else {
            assertEquals(tern, f.simplify)
          }
        case whoKnows =>
          if (tern == whoKnows) {
            (t.simplify, f.simplify) match {
              case (
                    Code.Const.One | Code.Const.True,
                    Code.Const.False | Code.Const.Zero
                  ) =>
                ()
              case tf =>
                fail(s"$tern == $whoKnows but (t,f) = $tf")
            }
          } else {
            (t.simplify, f.simplify) match {
              case (
                    Code.Const.False | Code.Const.Zero,
                    Code.Const.One | Code.Const.True
                  ) =>
                val not = Code.Not(whoKnows).simplify
                assertEquals(tern, not)
              case (ts, fs) =>
                assertEquals(tern, Code.Ternary(ts, whoKnows, fs))
            }
          }
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

    assertEquals(Code.toDoc(and).renderTrim(80), "(a == b) and (b == c)")
    assertEquals(Code
        .toDoc(Code.Ident("z").evalAnd(and))
        .renderTrim(80), "z and (a == b) and (b == c)")
  }

  test("simplify applies lambdas") {
    // (lambda x: f(x))(y) == f(y)

    // this is here so we don't shrink the args without the lambda, since
    // they need to be aligned
    case class LambdaArgs(lam: Code.Lambda, args: List[Code.Expression])

    val genArgs =
      for {
        n <- Gen.choose(0, 4)
        largs <- Gen.listOfN(n, genIdent)
        args <- Gen.listOfN(n, genIdent)
        result <- genExpr(4)
      } yield LambdaArgs(Code.Lambda(largs, result), args)

    forAll(genArgs) { case LambdaArgs(lam, arg) =>
      assertEquals(lam(arg*).simplify, Code
          .substitute(lam.args.zip(arg).toMap, lam.result)
          .simplify)
    }
  }

  test("(lambda x: lambda y: x + y)(y)") {
    val x = Code.Ident("x")
    val y = Code.Ident("y")
    val hardCase = Code.Lambda(List(x), Code.Lambda(List(y), x + y))

    val applied = hardCase(y).simplify
    val y0 = Code.Ident("y0")
    assertEquals(applied, Code.Lambda(List(y0), y + y0))

    val z = Code.Ident("z")
    val applied1 = hardCase(z).simplify
    assertEquals(applied1, Code.Lambda(List(y), z + y))
  }

  test("simplify(Map.empty, x) == x") {
    forAll(genExpr(4)) { x =>
      assertEquals(Code.substitute(Map.empty, x), x)
    }
  }

  test(
    "simplify creates subsets of freeIdents (we can remove ternary branches)"
  ) {
    forAll(genExpr(4)) { x =>
      assert(Code.freeIdents(x.simplify).subsetOf(Code.freeIdents(x)))
    }
  }
}
