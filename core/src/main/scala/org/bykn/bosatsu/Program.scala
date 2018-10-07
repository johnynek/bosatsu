package org.bykn.bosatsu

import cats.evidence.Is

case class Program[D, S](types: rankn.TypeEnv, lets: List[(String, D)], from: S) {
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
    nameToType: String => rankn.Type.Const,
    nameToCons: String => (PackageName, ConstructorName),
    stmt: Statement): Program[Expr[Declaration], Statement] = {

    import Statement._

    def declToE(d: Declaration): Expr[Declaration] =
      d.toExpr(nameToType, nameToCons)

    def defToT(
      types: rankn.TypeEnv,
      d: TypeDefinitionStatement): rankn.TypeEnv =
      types.addDefinedType(d.toDefinition(pn0, nameToType))

    def loop(s: Statement): Program[Expr[Declaration], Statement] =
      s match {
        case Bind(BindingStatement(nm, decl, Padding(_, rest))) =>
          val Program(te, binds, _) = loop(rest)
          Program(te, (nm, declToE(decl)) :: binds, stmt)
        case Comment(CommentStatement(_, Padding(_, on))) =>
          loop(on).copy(from = s)
        case Def(defstmt@DefStatement(_, _, _, _)) =>
          val (lam, Program(te, binds, _)) = defstmt.result match {
            case (Padding(_, Indented(_, body)), Padding(_, in)) =>
              // using body for the outer here is a bummer, but not really a good outer otherwise
              val l = defstmt.toLambdaExpr(declToE(body), body)(_.toNType(nameToType))
              (l, loop(in))
          }
          Program(te, (defstmt.name, lam) :: binds, stmt)
        case s@Struct(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = defToT(p.types, s), from = s)
        case e@Enum(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = defToT(p.types, e), from = e)
        case d@ExternalDef(name, _, _, Padding(_, rest)) =>
          val tpe = d.toType(nameToType)
           val p = loop(rest)
           p.copy(types = p.types.addExternalValue(name, tpe), from = d)
        case x@ExternalStruct(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = defToT(p.types, x), from = x)
        case EndOfFile =>
          Program(rankn.TypeEnv.empty, Nil, EndOfFile)
      }

    loop(stmt)
  }

}
