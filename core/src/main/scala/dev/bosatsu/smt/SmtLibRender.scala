package dev.bosatsu.smt

import org.typelevel.paiges.Doc

object SmtLibRender {
  import SmtCommand._
  import SmtExpr._

  private val symbolRegex =
    "^[A-Za-z_~!@\\$%^&*+=<>.?/\\-][A-Za-z0-9_~!@\\$%^&*+=<>.?/\\-]*$".r

  private def renderSymbol(symbol: String): String =
    if (symbolRegex.matches(symbol)) symbol
    else s"|${symbol.replace("|", "||")}|"

  private val Space: Doc = Doc.char(' ')

  private def symbolDoc(symbol: String): Doc =
    Doc.text(renderSymbol(symbol))

  private def paren(head: Doc, tail: Vector[Doc]): Doc = {
    val inside =
      if (tail.isEmpty) head
      else head + Space + Doc.intercalate(Space, tail)
    Doc.char('(') + inside + Doc.char(')')
  }

  private def variadic(op: String, args: Vector[Doc]): Doc =
    args match {
      case Vector(single) => single
      case _              => paren(Doc.text(op), args)
    }

  def renderSort(sort: SmtSort): Doc =
    sort match {
      case SmtSort.IntS  => Doc.text("Int")
      case SmtSort.BoolS => Doc.text("Bool")
    }

  def renderExpr[S <: SmtSort](expr: SmtExpr[S]): Doc =
    expr match {
      case IntConst(value) =>
        if (value.signum >= 0) Doc.text(value.toString)
        else paren(Doc.text("-"), Vector(Doc.text(value.abs.toString)))
      case BoolConst(value) =>
        if (value) Doc.text("true") else Doc.text("false")
      case Var(name) =>
        symbolDoc(name)
      case App(name, args) =>
        paren(symbolDoc(name), args.map(renderExpr(_)))
      case Ite(cond, ifTrue, ifFalse) =>
        paren(
          Doc.text("ite"),
          Vector(renderExpr(cond), renderExpr(ifTrue), renderExpr(ifFalse))
        )

      case Add(args) =>
        variadic("+", args.map(renderExpr(_)))
      case Sub(args) =>
        variadic("-", args.map(renderExpr(_)))
      case Mul(args) =>
        variadic("*", args.map(renderExpr(_)))
      case Div(num, den) =>
        paren(Doc.text("div"), Vector(renderExpr(num), renderExpr(den)))
      case Mod(num, den) =>
        paren(Doc.text("mod"), Vector(renderExpr(num), renderExpr(den)))

      case Lt(left, right) =>
        paren(Doc.text("<"), Vector(renderExpr(left), renderExpr(right)))
      case Lte(left, right) =>
        paren(Doc.text("<="), Vector(renderExpr(left), renderExpr(right)))
      case Gt(left, right) =>
        paren(Doc.text(">"), Vector(renderExpr(left), renderExpr(right)))
      case Gte(left, right) =>
        paren(Doc.text(">="), Vector(renderExpr(left), renderExpr(right)))
      case EqInt(left, right) =>
        paren(Doc.text("="), Vector(renderExpr(left), renderExpr(right)))
      case EqBool(left, right) =>
        paren(Doc.text("="), Vector(renderExpr(left), renderExpr(right)))

      case Not(inner) =>
        paren(Doc.text("not"), Vector(renderExpr(inner)))
      case And(args) =>
        variadic("and", args.map(renderExpr(_)))
      case Or(args)  =>
        variadic("or", args.map(renderExpr(_)))
      case Xor(left, right) =>
        paren(Doc.text("xor"), Vector(renderExpr(left), renderExpr(right)))
      case Implies(left, right) =>
        paren(Doc.text("=>"), Vector(renderExpr(left), renderExpr(right)))
    }

  def renderCommand(cmd: SmtCommand): Doc =
    cmd match {
      case SetLogic(logic) =>
        paren(Doc.text("set-logic"), Vector(symbolDoc(logic)))
      case DeclareConst(name, sort) =>
        paren(
          Doc.text("declare-const"),
          Vector(symbolDoc(name), renderSort(sort))
        )
      case DeclareFun(name, args, result) =>
        val argDoc =
          Doc.char('(') + Doc.intercalate(Space, args.map(renderSort)) + Doc.char(')')
        paren(
          Doc.text("declare-fun"),
          Vector(symbolDoc(name), argDoc, renderSort(result))
        )
      case DefineFun(name, args, result, body) =>
        val argDoc = args
          .map { case (argName, argSort) =>
            paren(symbolDoc(argName), Vector(renderSort(argSort)))
          }
        val argsDoc =
          Doc.char('(') + Doc.intercalate(Space, argDoc) + Doc.char(')')
        paren(
          Doc.text("define-fun"),
          Vector(
            symbolDoc(name),
            argsDoc,
            renderSort(result),
            renderExpr(body)
          )
        )
      case Assert(expr) =>
        paren(Doc.text("assert"), Vector(renderExpr(expr)))
      case CheckSat =>
        paren(Doc.text("check-sat"), Vector.empty)
      case GetModel =>
        paren(Doc.text("get-model"), Vector.empty)
    }

  def renderScriptDoc(script: SmtScript): Doc =
    Doc.intercalate(Doc.hardLine, script.commands.map(renderCommand)) + Doc.hardLine

  def renderScript(script: SmtScript): String =
    renderScriptDoc(script).render(80)
}
