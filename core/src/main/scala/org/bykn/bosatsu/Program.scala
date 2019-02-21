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
      Declaration.Binding(BindingStatement(Pattern.Var(nm), v, Padding(0, in)))(r)
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


    def bindings(b: Pattern[Option[String], TypeRef], decl: Expr[Declaration]): List[(String, Expr[Declaration])] =
      b match {
        case Pattern.Var(nm) => (nm, decl) :: Nil
        case unsupported => sys.error(s"unsupported pattern: $unsupported")
      }

    def loop(s: Statement): Program[Expr[Declaration], Statement] =
      s match {
        case Bind(BindingStatement(bound, decl, Padding(_, rest))) =>
          val Program(te, binds, _) = loop(rest)
          Program(te, bindings(bound, declToE(decl)) ::: binds, stmt)
        case Comment(CommentStatement(_, Padding(_, on))) =>
          loop(on).copy(from = s)
        case Def(defstmt@DefStatement(_, _, _, _)) =>
          val (lam, Program(te, binds, _)) = defstmt.result match {
            case (body, Padding(_, in)) =>
              // using body for the outer here is a bummer, but not really a good outer otherwise
              val l = defstmt.toLambdaExpr(declToE(body.get), body.get)(_.toType(nameToType))
              (l, loop(in))
          }
          Program(te, (defstmt.name, lam) :: binds, stmt)
        case s@Struct(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = defToT(p.types, s), from = s)
        case e@Enum(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = defToT(p.types, e), from = e)
        case ExternalDef(name, params, result, Padding(_, rest)) =>
          val tpe: rankn.Type = {
            def buildType(ts: List[rankn.Type]): rankn.Type =
              ts match {
                case Nil => result.toType(nameToType)
                case h :: tail => rankn.Type.Fun(h, buildType(tail))
              }
            buildType(params.map(_._2.toType(nameToType)))
          }
          val freeVars = rankn.Type.freeTyVars(tpe :: Nil)
          // these vars were parsed so they are never skolem vars
          val freeBound = freeVars.map {
            case b@rankn.Type.Var.Bound(_) => b
            case s@rankn.Type.Var.Skolem(_, _) => sys.error(s"invariant violation: parsed a skolem var: $s")
          }
          val maybeForAll = rankn.Type.forAll(freeBound, tpe)
          val p = loop(rest)
          p.copy(types = p.types.addExternalValue(pn0, name, maybeForAll), from = s)
        case x@ExternalStruct(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = defToT(p.types, x), from = x)
        case EndOfFile =>
          Program(rankn.TypeEnv.empty, Nil, EndOfFile)
      }

    loop(stmt)
  }

}
