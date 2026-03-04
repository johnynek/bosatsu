package dev.bosatsu.smt

object SmtScriptScope {
  private def undeclaredVarsInExpr(
      expr: SmtExpr[?],
      inScope: Set[String]
  ): Set[String] =
    expr match {
      case SmtExpr.IntConst(_) | SmtExpr.BoolConst(_) =>
        Set.empty
      case SmtExpr.Var(name)                          =>
        if (inScope(name)) Set.empty else Set(name)
      case SmtExpr.App(_, args)                       =>
        args.iterator.flatMap(undeclaredVarsInExpr(_, inScope)).toSet
      case SmtExpr.Ite(cond, ifTrue, ifFalse)         =>
        undeclaredVarsInExpr(cond, inScope) ++
          undeclaredVarsInExpr(ifTrue, inScope) ++
          undeclaredVarsInExpr(ifFalse, inScope)
      case SmtExpr.Add(args)                          =>
        args.iterator.flatMap(undeclaredVarsInExpr(_, inScope)).toSet
      case SmtExpr.Sub(args)                          =>
        args.iterator.flatMap(undeclaredVarsInExpr(_, inScope)).toSet
      case SmtExpr.Mul(args)                          =>
        args.iterator.flatMap(undeclaredVarsInExpr(_, inScope)).toSet
      case SmtExpr.Div(num, den)                      =>
        undeclaredVarsInExpr(num, inScope) ++ undeclaredVarsInExpr(den, inScope)
      case SmtExpr.Mod(num, den)                      =>
        undeclaredVarsInExpr(num, inScope) ++ undeclaredVarsInExpr(den, inScope)
      case SmtExpr.Lt(left, right)                    =>
        undeclaredVarsInExpr(left, inScope) ++ undeclaredVarsInExpr(right, inScope)
      case SmtExpr.Lte(left, right)                   =>
        undeclaredVarsInExpr(left, inScope) ++ undeclaredVarsInExpr(right, inScope)
      case SmtExpr.Gt(left, right)                    =>
        undeclaredVarsInExpr(left, inScope) ++ undeclaredVarsInExpr(right, inScope)
      case SmtExpr.Gte(left, right)                   =>
        undeclaredVarsInExpr(left, inScope) ++ undeclaredVarsInExpr(right, inScope)
      case SmtExpr.EqInt(left, right)                 =>
        undeclaredVarsInExpr(left, inScope) ++ undeclaredVarsInExpr(right, inScope)
      case SmtExpr.EqBool(left, right)                =>
        undeclaredVarsInExpr(left, inScope) ++ undeclaredVarsInExpr(right, inScope)
      case SmtExpr.Not(inner)                         =>
        undeclaredVarsInExpr(inner, inScope)
      case SmtExpr.And(args)                          =>
        args.iterator.flatMap(undeclaredVarsInExpr(_, inScope)).toSet
      case SmtExpr.Or(args)                           =>
        args.iterator.flatMap(undeclaredVarsInExpr(_, inScope)).toSet
      case SmtExpr.Xor(left, right)                   =>
        undeclaredVarsInExpr(left, inScope) ++ undeclaredVarsInExpr(right, inScope)
      case SmtExpr.Implies(left, right)               =>
        undeclaredVarsInExpr(left, inScope) ++ undeclaredVarsInExpr(right, inScope)
    }

  def undeclaredVars(script: SmtScript): Set[String] = {
    import SmtCommand.*

    val init: (Set[String], Set[String]) = (Set.empty, Set.empty)

    script.commands.foldLeft(init) {
      case ((declared, bad), DeclareConst(name, _)) =>
        (declared + name, bad)
      case ((declared, bad), DefineFun(_, args, _, body)) =>
        val localScope = declared ++ args.iterator.map(_._1)
        (declared, bad ++ undeclaredVarsInExpr(body, localScope.toSet))
      case ((declared, bad), Assert(expr)) =>
        (declared, bad ++ undeclaredVarsInExpr(expr, declared))
      case ((declared, bad), _) =>
        (declared, bad)
    }._2
  }
}
