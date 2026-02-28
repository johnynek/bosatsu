package dev.bosatsu.smt

object SmtToSExpr {
  import SExpr._
  import SmtCommand._
  import SmtExpr._

  private val symbolRegex =
    "^[A-Za-z_~!@\\$%^&*+=<>.?/\\-][A-Za-z0-9_~!@\\$%^&*+=<>.?/\\-]*$".r

  private def atom(value: String): SExpr =
    Atom(value)

  private def list(items: Vector[SExpr]): SExpr =
    List(items)

  private def s(head: String, tail: Vector[SExpr] = Vector.empty): SExpr =
    list(atom(head) +: tail)

  def escapedSymbol(symbol: String): String =
    if (symbolRegex.matches(symbol)) symbol
    else s"|${symbol.replace("|", "||")}|"

  private def symbol(symbol: String): SExpr =
    atom(escapedSymbol(symbol))

  private def variadic(op: String, args: Vector[SExpr]): SExpr =
    args match {
      case Vector(single) => single
      case _              => s(op, args)
    }

  def sort(sort: SmtSort): SExpr =
    sort match {
      case SmtSort.IntS  => atom("Int")
      case SmtSort.BoolS => atom("Bool")
    }

  def expr[S <: SmtSort](in: SmtExpr[S]): SExpr =
    in match {
      case IntConst(value) =>
        if (value.signum >= 0) atom(value.toString)
        else s("-", Vector(atom(value.abs.toString)))
      case BoolConst(value) =>
        atom(if (value) "true" else "false")
      case Var(name)       =>
        symbol(name)
      case App(name, args) =>
        s(escapedSymbol(name), args.map(expr(_)))
      case Ite(cond, ifTrue, ifFalse) =>
        s("ite", Vector(expr(cond), expr(ifTrue), expr(ifFalse)))

      case Add(args) =>
        variadic("+", args.map(expr(_)))
      case Sub(args) =>
        variadic("-", args.map(expr(_)))
      case Mul(args) =>
        variadic("*", args.map(expr(_)))
      case Div(num, den) =>
        s("div", Vector(expr(num), expr(den)))
      case Mod(num, den) =>
        s("mod", Vector(expr(num), expr(den)))

      case Lt(left, right) =>
        s("<", Vector(expr(left), expr(right)))
      case Lte(left, right) =>
        s("<=", Vector(expr(left), expr(right)))
      case Gt(left, right) =>
        s(">", Vector(expr(left), expr(right)))
      case Gte(left, right) =>
        s(">=", Vector(expr(left), expr(right)))
      case EqInt(left, right) =>
        s("=", Vector(expr(left), expr(right)))
      case EqBool(left, right) =>
        s("=", Vector(expr(left), expr(right)))

      case Not(inner) =>
        s("not", Vector(expr(inner)))
      case And(args) =>
        variadic("and", args.map(expr(_)))
      case Or(args)  =>
        variadic("or", args.map(expr(_)))
      case Xor(left, right) =>
        s("xor", Vector(expr(left), expr(right)))
      case Implies(left, right) =>
        s("=>", Vector(expr(left), expr(right)))
    }

  def command(cmd: SmtCommand): SExpr =
    cmd match {
      case SetLogic(logic) =>
        s("set-logic", Vector(symbol(logic)))
      case DeclareConst(name, sort1) =>
        s("declare-const", Vector(symbol(name), sort(sort1)))
      case DeclareFun(name, args, result) =>
        s(
          "declare-fun",
          Vector(
            symbol(name),
            list(args.map(sort)),
            sort(result)
          )
        )
      case DefineFun(name, args, result, body) =>
        s(
          "define-fun",
          Vector(
            symbol(name),
            list(args.map { case (argName, argSort) =>
              list(Vector(symbol(argName), sort(argSort)))
            }),
            sort(result),
            expr(body)
          )
        )
      case Assert(e) =>
        s("assert", Vector(expr(e)))
      case CheckSat =>
        s("check-sat")
      case GetModel =>
        s("get-model")
    }

  def script(script: SmtScript): Vector[SExpr] =
    script.commands.map(command)
}
