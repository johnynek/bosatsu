package dev.bosatsu.smt

/** Reads the obligations in core/src/test/resources/int_obligations: one per
  * line, as
  *
  * `(obligation (vars (name Sort) ...) (facts fact ...) (goal goal))`
  *
  * with terms in the SMT-LIB that SmtToSExpr renders.
  */
object IntObligationReader {
  import SmtExpr.*

  final case class Obligation(
      ints: Vector[String],
      bools: Vector[String],
      facts: Vector[BoolExpr],
      goal: BoolExpr
  )

  private def atom(name: String): SExpr = SExpr.Atom(name)

  private def unquote(symbol: String): String =
    if (symbol.length >= 2 && symbol.head == '|' && symbol.last == '|')
      symbol.substring(1, symbol.length - 1).replace("||", "|")
    else symbol

  private final class Terms(ints: Set[String], bools: Set[String]) {
    private def isNumeral(a: String): Boolean = a.nonEmpty && a.forall(_.isDigit)

    // only an Int and a Bool term are compared with `=`, so the sort of the
    // left side decides which
    private def isInt(e: SExpr): Boolean =
      e match {
        case SExpr.Atom(a)                       => isNumeral(a) || ints(unquote(a))
        case SExpr.List(SExpr.Atom(op) +: rest) =>
          op match {
            case "+" | "-" | "*" | "div" | "mod" => true
            case "ite"                           => rest.lift(1).exists(isInt)
            case _                               => false
          }
        case _                                   => false
      }

    def int(e: SExpr): Either[String, IntExpr] =
      e match {
        case SExpr.Atom(a) if isNumeral(a)               => Right(IntConst(BigInt(a)))
        case SExpr.Atom(a) if ints(unquote(a))           => Right(Var[SmtSort.IntSort](unquote(a)))
        // SmtToSExpr renders a negative constant as `(- k)`, and a
        // one-argument Sub as its argument, so `(- a)` is negation
        case SExpr.List(Vector(SExpr.Atom("-"), SExpr.Atom(a))) if isNumeral(a) =>
          Right(IntConst(-BigInt(a)))
        case SExpr.List(Vector(SExpr.Atom("-"), a))      =>
          int(a).map(x => Sub(Vector(IntConst(BigInt(0)), x)))
        case SExpr.List(SExpr.Atom("+") +: args)        => ints(args).map(Add(_))
        case SExpr.List(SExpr.Atom("-") +: args)        => ints(args).map(Sub(_))
        case SExpr.List(SExpr.Atom("*") +: args)        => ints(args).map(Mul(_))
        case SExpr.List(Vector(SExpr.Atom("div"), a, b)) =>
          for { x <- int(a); y <- int(b) } yield Div(x, y)
        case SExpr.List(Vector(SExpr.Atom("mod"), a, b)) =>
          for { x <- int(a); y <- int(b) } yield Mod(x, y)
        case SExpr.List(Vector(SExpr.Atom("ite"), p, a, b)) =>
          for { c <- bool(p); x <- int(a); y <- int(b) } yield Ite[SmtSort.IntSort](c, x, y)
        case other                                       => Left(s"not an Int term: $other")
      }

    private def ints(args: Vector[SExpr]): Either[String, Vector[IntExpr]] =
      args.foldLeft(Right(Vector.empty): Either[String, Vector[IntExpr]]) { (acc, a) =>
        acc.flatMap(xs => int(a).map(xs :+ _))
      }

    private def boolArgs(args: Vector[SExpr]): Either[String, Vector[BoolExpr]] =
      args.foldLeft(Right(Vector.empty): Either[String, Vector[BoolExpr]]) { (acc, a) =>
        acc.flatMap(xs => bool(a).map(xs :+ _))
      }

    private def cmp(a: SExpr, b: SExpr)(mk: (IntExpr, IntExpr) => BoolExpr): Either[String, BoolExpr] =
      for { x <- int(a); y <- int(b) } yield mk(x, y)

    private def two(a: SExpr, b: SExpr)(mk: (BoolExpr, BoolExpr) => BoolExpr): Either[String, BoolExpr] =
      for { x <- bool(a); y <- bool(b) } yield mk(x, y)

    def bool(e: SExpr): Either[String, BoolExpr] =
      e match {
        case SExpr.Atom("true")                          => Right(BoolConst.True)
        case SExpr.Atom("false")                         => Right(BoolConst.False)
        case SExpr.Atom(a) if bools(unquote(a))          => Right(Var[SmtSort.BoolSort](unquote(a)))
        case SExpr.List(Vector(SExpr.Atom("<"), a, b))   => cmp(a, b)(Lt(_, _))
        case SExpr.List(Vector(SExpr.Atom("<="), a, b))  => cmp(a, b)(Lte(_, _))
        case SExpr.List(Vector(SExpr.Atom(">"), a, b))   => cmp(a, b)(Gt(_, _))
        case SExpr.List(Vector(SExpr.Atom(">="), a, b))  => cmp(a, b)(Gte(_, _))
        case SExpr.List(Vector(SExpr.Atom("="), a, b))   =>
          if (isInt(a)) cmp(a, b)(EqInt(_, _)) else two(a, b)(EqBool(_, _))
        case SExpr.List(Vector(SExpr.Atom("not"), a))    => bool(a).map(Not(_))
        case SExpr.List(SExpr.Atom("and") +: args)       => boolArgs(args).map(And(_))
        case SExpr.List(SExpr.Atom("or") +: args)        => boolArgs(args).map(Or(_))
        case SExpr.List(Vector(SExpr.Atom("xor"), a, b)) => two(a, b)(Xor(_, _))
        case SExpr.List(Vector(SExpr.Atom("=>"), a, b))  => two(a, b)(Implies(_, _))
        case SExpr.List(Vector(SExpr.Atom("ite"), p, a, b)) =>
          for { c <- bool(p); x <- bool(a); y <- bool(b) } yield Ite[SmtSort.BoolSort](c, x, y)
        case other                                       => Left(s"not a Bool term: $other")
      }
  }

  def obligation(e: SExpr): Either[String, Obligation] =
    e match {
      case SExpr.List(
            Vector(
              SExpr.Atom("obligation"),
              SExpr.List(SExpr.Atom("vars") +: vars),
              SExpr.List(SExpr.Atom("facts") +: facts),
              SExpr.List(Vector(SExpr.Atom("goal"), goal))
            )
          ) =>
        val sorts = vars.map {
          case SExpr.List(Vector(SExpr.Atom(name), sort)) => Right((unquote(name), sort))
          case other                                      => Left(s"not a declaration: $other")
        }
        sorts.collectFirst { case Left(err) => err } match {
          case Some(err) => Left(err)
          case None      =>
            val decls = sorts.collect { case Right(d) => d }
            val ints = decls.collect { case (n, s) if s == atom("Int") => n }
            val bools = decls.collect { case (n, s) if s == atom("Bool") => n }
            val terms = new Terms(ints.toSet, bools.toSet)
            for {
              fs <- facts.foldLeft(Right(Vector.empty): Either[String, Vector[BoolExpr]]) { (acc, f) =>
                acc.flatMap(xs => terms.bool(f).map(xs :+ _))
              }
              g <- terms.bool(goal)
            } yield Obligation(ints, bools, fs, g)
        }
      case other => Left(s"not an obligation: $other")
    }

  def parseAll(text: String): Either[String, Vector[Obligation]] =
    SExprParser.parseAll(text) match {
      case Left(err)    => Left(err.message)
      case Right(exprs) =>
        exprs.foldLeft(Right(Vector.empty): Either[String, Vector[Obligation]]) { (acc, e) =>
          acc.flatMap(os => obligation(e).map(os :+ _))
        }
    }
}
