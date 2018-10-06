package org.bykn.bosatsu

import cats.evidence.Is

case class Program[D, S](types: TypeEnv, lets: List[(String, D)], from: S) {
  private[this] lazy val letMap: Map[String, D] = lets.toMap

  def getLet(name: String): Option[D] = letMap.get(name)
  /**
   * main is the thing we evaluate. It is the last thing defined
   */
  def getMain[D1](fn: (String, D1, D1) => D1)(implicit ev: D Is Expr[D1]): Option[Expr[D1]] = {
    @annotation.tailrec
    def loop(ls: List[(String, Expr[D1])], acc: Expr[D1]): Expr[D1] =
      ls match {
        case Nil => acc
        case (nm, expr) :: tail =>
          val decl = fn(nm, expr.tag, acc.tag)
          loop(tail, Expr.Let(nm, expr, acc, decl))
      }

    lets.reverse match {
      case (_, h) :: tail =>
        type L[X] = List[(String, X)]
        Some(loop(ev.substitute[L](tail), ev.coerce(h)))
      case Nil =>
        None
    }
  }

  def getMainDecl(implicit ev: D Is Expr[Declaration]): Option[Expr[Declaration]] = {

    val fn = { (nm: String, v: Declaration, in: Declaration) =>
      val r = v.region + in.region
      Declaration.Binding(BindingStatement(nm, v, Padding(0, in)))(r)
    }

    getMain[Declaration](fn)
  }
}

object Program {
  def fromStatement(
    pn0: PackageName,
    importMap: ImportMap[PackageName, Unit],
    stmt: Statement): Program[Expr[Declaration], Statement] = {

    import Statement._
    // TODO use the importMap to see what names come from where

    def loop(s: Statement): Program[Expr[Declaration], Statement] =
      s match {
        case Bind(BindingStatement(nm, decl, Padding(_, rest))) =>
          val Program(te, binds, _) = loop(rest)
          Program(te, (nm, decl.toExpr(pn0, importMap)) :: binds, stmt)
        case Comment(CommentStatement(_, Padding(_, on))) =>
          loop(on).copy(from = s)
        case Def(defstmt@DefStatement(_, _, _, _)) =>
          val (lam, Program(te, binds, _)) = defstmt.result match {
            case (Padding(_, Indented(_, body)), Padding(_, in)) =>
              // using body for the outer here is a bummer, but not really a good outer otherwise
              val l = defstmt.toLambdaExpr(body.toExpr(pn0, importMap), body)(_.toNType(pn0, importMap))
              (l, loop(in))
          }
          Program(te, (defstmt.name, lam) :: binds, stmt)
        case s@Struct(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = p.types.addDefinedType(s.toDefinition(pn0, importMap)), from = s)
        case e@Enum(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = p.types.addDefinedType(e.toDefinition(pn0, importMap)), from = e)
        case d@ExternalDef(name, _, _, Padding(_, rest)) =>
           val scheme = d.scheme(pn0, importMap)
           val p = loop(rest)
           p.copy(types = p.types.updated(name, scheme), from = d)
        case x@ExternalStruct(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = p.types.addDefinedType(x.toDefinition(pn0, importMap)), from = x)
        case EndOfFile =>
          Program(TypeEnv.empty(pn0, importMap), Nil, EndOfFile)
      }

    loop(stmt)
  }

}
