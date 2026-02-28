package dev.bosatsu.smt

object SmtLibRender {
  import SmtCommand._
  import SmtExpr._

  private val symbolRegex =
    "^[A-Za-z_~!@\\$%^&*+=<>.?/\\-][A-Za-z0-9_~!@\\$%^&*+=<>.?/\\-]*$".r

  private def renderSymbol(symbol: String): String =
    if (symbolRegex.matches(symbol)) symbol
    else s"|${symbol.replace("|", "||")}|"

  private def paren(head: String, tail: Vector[String]): String =
    if (tail.isEmpty) s"($head)"
    else s"($head ${tail.mkString(" ")})"

  private def variadic(op: String, args: Vector[String]): String =
    args match {
      case Vector(single) => single
      case _              => paren(op, args)
    }

  def renderSort(sort: SmtSort): String =
    sort match {
      case SmtSort.IntS  => "Int"
      case SmtSort.BoolS => "Bool"
    }

  def renderExpr[S <: SmtSort](expr: SmtExpr[S]): String =
    expr match {
      case IntConst(value) =>
        if (value.signum >= 0) value.toString
        else s"(- ${value.abs})"
      case BoolConst(value) =>
        if (value) "true" else "false"
      case Var(name) =>
        renderSymbol(name)
      case App(name, args) =>
        paren(renderSymbol(name), args.map(renderExpr(_)))
      case Ite(cond, ifTrue, ifFalse) =>
        paren(
          "ite",
          Vector(renderExpr(cond), renderExpr(ifTrue), renderExpr(ifFalse))
        )

      case Add(args) =>
        variadic("+", args.map(renderExpr(_)))
      case Sub(args) =>
        variadic("-", args.map(renderExpr(_)))
      case Mul(args) =>
        variadic("*", args.map(renderExpr(_)))
      case Div(num, den) =>
        paren("div", Vector(renderExpr(num), renderExpr(den)))
      case Mod(num, den) =>
        paren("mod", Vector(renderExpr(num), renderExpr(den)))

      case Lt(left, right) =>
        paren("<", Vector(renderExpr(left), renderExpr(right)))
      case Lte(left, right) =>
        paren("<=", Vector(renderExpr(left), renderExpr(right)))
      case Gt(left, right) =>
        paren(">", Vector(renderExpr(left), renderExpr(right)))
      case Gte(left, right) =>
        paren(">=", Vector(renderExpr(left), renderExpr(right)))
      case EqInt(left, right) =>
        paren("=", Vector(renderExpr(left), renderExpr(right)))
      case EqBool(left, right) =>
        paren("=", Vector(renderExpr(left), renderExpr(right)))

      case Not(inner) =>
        paren("not", Vector(renderExpr(inner)))
      case And(args) =>
        variadic("and", args.map(renderExpr(_)))
      case Or(args)  =>
        variadic("or", args.map(renderExpr(_)))
      case Xor(left, right) =>
        paren("xor", Vector(renderExpr(left), renderExpr(right)))
      case Implies(left, right) =>
        paren("=>", Vector(renderExpr(left), renderExpr(right)))
    }

  def renderCommand(cmd: SmtCommand): String =
    cmd match {
      case SetLogic(logic) =>
        paren("set-logic", Vector(renderSymbol(logic)))
      case DeclareConst(name, sort) =>
        paren(
          "declare-const",
          Vector(renderSymbol(name), renderSort(sort))
        )
      case DeclareFun(name, args, result) =>
        val argDoc = s"(${args.map(renderSort).mkString(" ")})"
        paren(
          "declare-fun",
          Vector(renderSymbol(name), argDoc, renderSort(result))
        )
      case DefineFun(name, args, result, body) =>
        val argDoc = args
          .map { case (argName, argSort) =>
            s"(${renderSymbol(argName)} ${renderSort(argSort)})"
          }
          .mkString("(", " ", ")")
        paren(
          "define-fun",
          Vector(
            renderSymbol(name),
            argDoc,
            renderSort(result),
            renderExpr(body)
          )
        )
      case Assert(expr) =>
        paren("assert", Vector(renderExpr(expr)))
      case CheckSat =>
        "(check-sat)"
      case GetModel =>
        "(get-model)"
    }

  def renderScript(script: SmtScript): String =
    script.commands.iterator.map(renderCommand).mkString("", "\n", "\n")
}
