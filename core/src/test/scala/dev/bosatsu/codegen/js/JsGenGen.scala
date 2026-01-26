package dev.bosatsu.codegen.js

import org.scalacheck.{Gen, Arbitrary}
import cats.data.NonEmptyList

/**
 * ScalaCheck generators for JavaScript Code AST types.
 * Used for property-based testing of JsGen and Code rendering.
 */
object JsGenGen {

  // ==================
  // Identifier Generators
  // ==================

  /** Generate a valid JavaScript identifier first character */
  val genIdentFirstChar: Gen[Char] =
    Gen.frequency(
      (26, Gen.oneOf('a' to 'z')),
      (26, Gen.oneOf('A' to 'Z')),
      (1, Gen.const('_')),
      (1, Gen.const('$'))
    )

  /** Generate a valid JavaScript identifier continuation character */
  val genIdentChar: Gen[Char] =
    Gen.frequency(
      (26, Gen.oneOf('a' to 'z')),
      (26, Gen.oneOf('A' to 'Z')),
      (10, Gen.oneOf('0' to '9')),
      (1, Gen.const('_')),
      (1, Gen.const('$'))
    )

  /** Generate a valid JavaScript identifier name (not a reserved word) */
  val genIdentName: Gen[String] = for {
    first <- genIdentFirstChar
    restLen <- Gen.choose(0, 10)
    rest <- Gen.listOfN(restLen, genIdentChar)
    name = (first :: rest).mkString
    // Filter out reserved words
    validName <- if (jsReservedWords.contains(name)) genIdentName else Gen.const(name)
  } yield validName

  /** Generate a Code.Ident */
  val genIdent: Gen[Code.Ident] = genIdentName.map(Code.Ident(_))

  /** JavaScript reserved words to avoid */
  val jsReservedWords: Set[String] = Set(
    "break", "case", "catch", "class", "const", "continue", "debugger", "default",
    "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for",
    "function", "if", "import", "in", "instanceof", "let", "new", "null", "return",
    "static", "super", "switch", "this", "throw", "true", "try", "typeof", "undefined",
    "var", "void", "while", "with", "yield"
  )

  // ==================
  // Literal Generators
  // ==================

  /** Generate a safe integer literal (fits in JS number safely) */
  val genIntLiteral: Gen[Code.IntLiteral] =
    Gen.choose(-1000000L, 1000000L).map(i => Code.IntLiteral(BigInt(i)))

  /** Generate a double literal */
  val genDoubleLiteral: Gen[Code.DoubleLiteral] =
    Gen.choose(-1000.0, 1000.0).map(Code.DoubleLiteral(_))

  /** Generate a string that's safe for JS string literals */
  val genSafeString: Gen[String] =
    Gen.listOf(Gen.frequency(
      (90, Gen.alphaNumChar),
      (5, Gen.oneOf(' ', ',', '.', '!', '?', '-', '_')),
      (5, Gen.const(' '))
    )).map(_.mkString)

  /** Generate a string literal */
  val genStringLiteral: Gen[Code.StringLiteral] =
    genSafeString.map(Code.StringLiteral(_))

  /** Generate a boolean literal */
  val genBoolLiteral: Gen[Code.BoolLiteral] =
    Gen.oneOf(Code.TrueLit, Code.FalseLit)

  /** Generate any simple literal */
  val genSimpleLiteral: Gen[Code.Literal] =
    Gen.oneOf(
      genIntLiteral,
      genDoubleLiteral,
      genStringLiteral,
      genBoolLiteral,
      Gen.const(Code.NullLiteral),
      Gen.const(Code.UndefinedLiteral)
    )

  // ==================
  // Expression Generators
  // ==================

  /** Generate an atomic expression (no subexpressions) */
  val genAtomicExpr: Gen[Code.Expression] =
    Gen.oneOf(genIdent, genSimpleLiteral)

  /** Generate an array literal */
  def genArrayLiteral(depth: Int): Gen[Code.ArrayLiteral] = for {
    size <- Gen.choose(0, 5)
    elements <- Gen.listOfN(size, genExpressionWithDepth(depth - 1))
  } yield Code.ArrayLiteral(elements)

  /** Generate an object literal */
  def genObjectLiteral(depth: Int): Gen[Code.ObjectLiteral] = for {
    size <- Gen.choose(0, 4)
    keys <- Gen.listOfN(size, genIdentName)
    values <- Gen.listOfN(size, genExpressionWithDepth(depth - 1))
  } yield Code.ObjectLiteral(keys.zip(values))

  /** Generate a binary operator */
  val genBinOp: Gen[Code.BinOp] =
    Gen.oneOf(
      Code.BinOp.Plus, Code.BinOp.Minus, Code.BinOp.Times, Code.BinOp.Div, Code.BinOp.Mod,
      Code.BinOp.Eq, Code.BinOp.NotEq, Code.BinOp.Lt, Code.BinOp.LtEq, Code.BinOp.Gt, Code.BinOp.GtEq,
      Code.BinOp.And, Code.BinOp.Or
    )

  /** Generate a prefix operator */
  val genPrefixOp: Gen[Code.PrefixOp] =
    Gen.oneOf(Code.PrefixOp.Not, Code.PrefixOp.Neg, Code.PrefixOp.Pos, Code.PrefixOp.TypeOf)

  /** Generate a binary expression */
  def genBinExpr(depth: Int): Gen[Code.BinExpr] = for {
    left <- genExpressionWithDepth(depth - 1)
    op <- genBinOp
    right <- genExpressionWithDepth(depth - 1)
  } yield Code.BinExpr(left, op, right)

  /** Generate a prefix expression */
  def genPrefixExpr(depth: Int): Gen[Code.PrefixExpr] = for {
    op <- genPrefixOp
    expr <- genExpressionWithDepth(depth - 1)
  } yield Code.PrefixExpr(op, expr)

  /** Generate a ternary expression */
  def genTernary(depth: Int): Gen[Code.Ternary] = for {
    cond <- genExpressionWithDepth(depth - 1)
    t <- genExpressionWithDepth(depth - 1)
    f <- genExpressionWithDepth(depth - 1)
  } yield Code.Ternary(cond, t, f)

  /** Generate a function call */
  def genCall(depth: Int): Gen[Code.Call] = for {
    fn <- Gen.oneOf(genIdent, genPropertyAccess(depth - 1))
    argCount <- Gen.choose(0, 4)
    args <- Gen.listOfN(argCount, genExpressionWithDepth(depth - 1))
  } yield Code.Call(fn, args)

  /** Generate a property access */
  def genPropertyAccess(depth: Int): Gen[Code.PropertyAccess] = for {
    obj <- genExpressionWithDepth(depth - 1)
    prop <- genIdentName
  } yield Code.PropertyAccess(obj, prop)

  /** Generate an index access */
  def genIndexAccess(depth: Int): Gen[Code.IndexAccess] = for {
    obj <- genExpressionWithDepth(depth - 1)
    idx <- Gen.oneOf(genIntLiteral.map(_.asInstanceOf[Code.Expression]), genStringLiteral.map(_.asInstanceOf[Code.Expression]))
  } yield Code.IndexAccess(obj, idx)

  /** Generate an arrow function */
  def genArrowFunction(depth: Int): Gen[Code.ArrowFunction] = for {
    paramCount <- Gen.choose(0, 4)
    params <- Gen.listOfN(paramCount, genIdentName)
    body <- Gen.oneOf(
      genExpressionWithDepth(depth - 1).map(Left(_)),
      genBlock(depth - 1).map(Right(_))
    )
  } yield Code.ArrowFunction(params, body)

  /** Generate an expression with bounded recursion depth */
  def genExpressionWithDepth(depth: Int): Gen[Code.Expression] =
    if (depth <= 0) genAtomicExpr
    else Gen.frequency(
      (10, genAtomicExpr),
      (2, genArrayLiteral(depth)),
      (2, genObjectLiteral(depth)),
      (2, genBinExpr(depth)),
      (1, genPrefixExpr(depth)),
      (1, genTernary(depth)),
      (2, genCall(depth)),
      (2, genPropertyAccess(depth)),
      (1, genArrowFunction(depth))
    )

  /** Generate an expression with default depth */
  val genExpression: Gen[Code.Expression] = genExpressionWithDepth(3)

  // ==================
  // Statement Generators
  // ==================

  /** Generate a const declaration */
  def genConst(depth: Int): Gen[Code.Const] = for {
    name <- genIdentName
    value <- genExpressionWithDepth(depth)
  } yield Code.Const(name, value)

  /** Generate a let declaration */
  def genLet(depth: Int): Gen[Code.Let] = for {
    name <- genIdentName
    value <- Gen.option(genExpressionWithDepth(depth))
  } yield Code.Let(name, value)

  /** Generate a return statement */
  def genReturn(depth: Int): Gen[Code.Return] =
    Gen.option(genExpressionWithDepth(depth)).map(Code.Return(_))

  /** Generate an if statement */
  def genIfStatement(depth: Int): Gen[Code.IfStatement] = for {
    cond <- genExpressionWithDepth(depth)
    thenBlock <- genBlock(depth - 1)
    hasElse <- Gen.oneOf(true, false)
    elseBlock <- if (hasElse && depth > 0) Gen.some(genBlock(depth - 1).map(Right(_))) else Gen.const(None)
  } yield Code.IfStatement(cond, thenBlock, elseBlock)

  /** Generate an expression statement */
  def genExprStatement(depth: Int): Gen[Code.ExprStatement] =
    genExpressionWithDepth(depth).map(Code.ExprStatement(_))

  /** Generate a statement with bounded depth */
  def genStatementWithDepth(depth: Int): Gen[Code.Statement] =
    if (depth <= 0) Gen.oneOf(genReturn(0), genExprStatement(0))
    else Gen.frequency(
      (3, genConst(depth)),
      (2, genLet(depth)),
      (2, genReturn(depth)),
      (1, genIfStatement(depth)),
      (3, genExprStatement(depth))
    )

  /** Generate a statement with default depth */
  val genStatement: Gen[Code.Statement] = genStatementWithDepth(2)

  /** Generate a block of statements */
  def genBlock(depth: Int): Gen[Code.Block] = for {
    count <- Gen.choose(1, 4)
    stmts <- Gen.listOfN(count, genStatementWithDepth(depth))
    nel = NonEmptyList.fromListUnsafe(stmts)
  } yield Code.Block(nel)

  // ==================
  // Top-level Code Generators
  // ==================

  /** Generate either an expression or statement */
  val genCode: Gen[Code] = Gen.oneOf(genExpression, genStatement)

  // ==================
  // Arbitrary instances for implicit usage
  // ==================

  implicit val arbIdent: Arbitrary[Code.Ident] = Arbitrary(genIdent)
  implicit val arbExpression: Arbitrary[Code.Expression] = Arbitrary(genExpression)
  implicit val arbStatement: Arbitrary[Code.Statement] = Arbitrary(genStatement)
  implicit val arbLiteral: Arbitrary[Code.Literal] = Arbitrary(genSimpleLiteral)
}
