package dev.bosatsu.smt

import org.typelevel.paiges.Doc

object SmtLibRender {
  def renderSort(sort: SmtSort): Doc =
    SExpr.toDoc(SmtToSExpr.sort(sort))

  def renderExpr[S <: SmtSort](expr: SmtExpr[S]): Doc =
    SExpr.toDoc(SmtToSExpr.expr(expr))

  def renderCommand(cmd: SmtCommand): Doc =
    SExpr.toDoc(SmtToSExpr.command(cmd))

  def renderScriptDoc(script: SmtScript): Doc =
    SExpr.toDocAll(SmtToSExpr.script(script)) + Doc.hardLine

  def renderScript(script: SmtScript): String =
    renderScriptDoc(script).render(80)
}
