package dev.bosatsu.simulation

import dev.bosatsu.{Identifier, Matchless, Lit, PackageName}
import dev.bosatsu.Identifier.Bindable

/**
 * Analyzes Matchless IR to extract derivation information.
 *
 * This powers the "Why?" explanations by tracking:
 * - Dependencies: Which bindings does this value depend on?
 * - Kind: Is this an Assumption (no deps), Computation (has deps), or Conditional?
 * - Formula: Human-readable representation of the computation
 */
object DerivationAnalyzer {

  /**
   * Derivation kind determines UI treatment.
   */
  sealed trait DerivationKind
  case object Assumption extends DerivationKind   // No local deps (input value)
  case object Computation extends DerivationKind  // Has deps (computed value)
  case object Conditional extends DerivationKind  // Contains If expression

  /**
   * Result of analyzing a binding.
   */
  case class AnalyzedBinding(
      name: Bindable,
      kind: DerivationKind,
      dependencies: Set[Bindable],
      formula: String,
      valueType: String  // "number", "boolean", "string"
  )

  /**
   * Analyze a single binding to extract derivation info.
   */
  def analyzeBinding[A](
      name: Bindable,
      expr: Matchless.Expr[A]
  ): AnalyzedBinding = {
    val deps = extractDependencies(expr)
    val kind = classifyKind(expr, deps)
    val formula = exprToFormula(expr)
    val valueType = inferType(expr)

    AnalyzedBinding(name, kind, deps, formula, valueType)
  }

  /**
   * Analyze multiple bindings, filtering deps to only include defined bindings.
   */
  def analyze[A](
      bindings: List[(Bindable, Matchless.Expr[A])]
  ): List[AnalyzedBinding] = {
    val scope = bindings.map(_._1).toSet
    bindings.map { case (name, expr) =>
      val allDeps = extractDependencies(expr)
      val localDeps = allDeps.intersect(scope)
      val kind = classifyKind(expr, localDeps)
      val formula = exprToFormula(expr)
      val valueType = inferType(expr)

      AnalyzedBinding(name, kind, localDeps, formula, valueType)
    }
  }

  /**
   * Classify the kind based on expression structure and dependencies.
   */
  private def classifyKind[A](
      expr: Matchless.Expr[A],
      deps: Set[Bindable]
  ): DerivationKind = {
    if (containsConditional(expr)) Conditional
    else if (deps.isEmpty) Assumption
    else Computation
  }

  /**
   * Check if expression contains an If.
   */
  private def containsConditional[A](expr: Matchless.Expr[A]): Boolean =
    expr match {
      case Matchless.If(_, _, _)      => true
      case Matchless.Let(_, v, b)     => containsConditional(v) || containsConditional(b)
      case Matchless.LetMut(_, b)     => containsConditional(b)
      case Matchless.Always(_, e)     => containsConditional(e)
      case Matchless.App(f, as)       => containsConditional(f) || as.exists(containsConditional)
      case Matchless.Lambda(_, _, _, b) => containsConditional(b)
      case Matchless.WhileExpr(_, e, _) => containsConditional(e)
      case _                          => false
    }

  /**
   * Extract all Bindable dependencies from an expression.
   */
  def extractDependencies[A](expr: Matchless.Expr[A]): Set[Bindable] =
    expr match {
      case Matchless.Local(name) =>
        Set(name)

      case Matchless.Let(Right(name), value, body) =>
        extractDependencies(value) ++ (extractDependencies(body) - name)

      case Matchless.Let(Left(_), value, body) =>
        extractDependencies(value) ++ extractDependencies(body)

      case Matchless.LetMut(_, body) =>
        extractDependencies(body)

      case Matchless.App(fn, args) =>
        extractDependencies(fn) ++ args.toList.flatMap(extractDependencies).toSet

      case Matchless.If(cond, thenE, elseE) =>
        extractBoolDeps(cond) ++ extractDependencies(thenE) ++ extractDependencies(elseE)

      case Matchless.Always(cond, result) =>
        extractBoolDeps(cond) ++ extractDependencies(result)

      case Matchless.Lambda(captures, _, args, body) =>
        captures.flatMap(extractDependencies).toSet ++
          (extractDependencies(body) -- args.toList.toSet)

      case Matchless.WhileExpr(cond, effect, _) =>
        extractBoolDeps(cond) ++ extractDependencies(effect)

      case Matchless.PrevNat(n) =>
        extractDependencies(n)

      case Matchless.GetEnumElement(arg, _, _, _) =>
        extractDependencies(arg)

      case Matchless.GetStructElement(arg, _, _) =>
        extractDependencies(arg)

      case Matchless.Global(_, _, _) |
           Matchless.LocalAnon(_) |
           Matchless.LocalAnonMut(_) |
           Matchless.ClosureSlot(_) |
           Matchless.Literal(_) |
           Matchless.MakeEnum(_, _, _) |
           Matchless.MakeStruct(_) |
           Matchless.ZeroNat |
           Matchless.SuccNat =>
        Set.empty
    }

  /**
   * Extract dependencies from a boolean expression.
   */
  def extractBoolDeps[A](bexpr: Matchless.BoolExpr[A]): Set[Bindable] =
    bexpr match {
      case Matchless.TrueConst =>
        Set.empty

      case Matchless.EqualsLit(expr, _) =>
        extractDependencies(expr)

      case Matchless.EqualsNat(expr, _) =>
        extractDependencies(expr)

      case Matchless.And(e1, e2) =>
        extractBoolDeps(e1) ++ extractBoolDeps(e2)

      case Matchless.CheckVariant(expr, _, _, _) =>
        extractDependencies(expr)

      case Matchless.SetMut(_, e) =>
        extractDependencies(e)

      case Matchless.MatchString(arg, _, _, _) =>
        extractDependencies(arg)

      case Matchless.LetBool(Right(name), value, in) =>
        extractDependencies(value) ++ (extractBoolDeps(in) - name)

      case Matchless.LetBool(Left(_), value, in) =>
        extractDependencies(value) ++ extractBoolDeps(in)

      case Matchless.LetMutBool(_, in) =>
        extractBoolDeps(in)
    }

  /**
   * Convert IR back to human-readable formula.
   *
   * This produces explanations like:
   * - App(Global("add"), [a, b]) -> "a + b"
   * - App(Global("times"), [x, y]) -> "x * y"
   */
  def exprToFormula[A](expr: Matchless.Expr[A]): String =
    expr match {
      case Matchless.Literal(lit) =>
        lit match {
          case Lit.Integer(i) => i.toString
          case Lit.Str(s) => s""""$s""""
          case Lit.Chr(c) => s"'$c'"
        }

      case Matchless.Local(name) =>
        name.asString

      case Matchless.Global(_, pack, name) =>
        if (pack == PackageName.PredefName) name.asString
        else s"${pack.asString}::${name.asString}"

      case Matchless.LocalAnon(id) =>
        s"_anon$id"

      case Matchless.LocalAnonMut(id) =>
        s"_mut$id"

      case Matchless.ClosureSlot(idx) =>
        s"_slot$idx"

      case Matchless.App(fn, args) =>
        // Try to convert predef functions to infix operators
        fn match {
          case Matchless.Global(_, PackageName.PredefName, name) =>
            val argFormulas = args.toList.map(exprToFormula)
            name.asString match {
              case "add" if argFormulas.size == 2 =>
                s"(${argFormulas(0)} + ${argFormulas(1)})"
              case "sub" if argFormulas.size == 2 =>
                s"(${argFormulas(0)} - ${argFormulas(1)})"
              case "times" if argFormulas.size == 2 =>
                s"(${argFormulas(0)} * ${argFormulas(1)})"
              case "div" if argFormulas.size == 2 =>
                s"(${argFormulas(0)} / ${argFormulas(1)})"
              case "mod_Int" if argFormulas.size == 2 =>
                s"(${argFormulas(0)} % ${argFormulas(1)})"
              case "eq_Int" if argFormulas.size == 2 =>
                s"(${argFormulas(0)} == ${argFormulas(1)})"
              case "cmp_Int" if argFormulas.size == 2 =>
                s"compare(${argFormulas.mkString(", ")})"
              case other =>
                s"$other(${argFormulas.mkString(", ")})"
            }
          case _ =>
            val fnFormula = exprToFormula(fn)
            val argFormulas = args.toList.map(exprToFormula)
            s"$fnFormula(${argFormulas.mkString(", ")})"
        }

      case Matchless.Let(Right(name), value, body) =>
        val vFormula = exprToFormula(value)
        val bFormula = exprToFormula(body)
        s"let ${name.asString} = $vFormula in $bFormula"

      case Matchless.Let(Left(la), value, body) =>
        val vFormula = exprToFormula(value)
        val bFormula = exprToFormula(body)
        s"let _anon${la.ident} = $vFormula in $bFormula"

      case Matchless.LetMut(Matchless.LocalAnonMut(id), body) =>
        s"let mut _mut$id in ${exprToFormula(body)}"

      case Matchless.If(cond, thenE, elseE) =>
        s"if ${boolExprToFormula(cond)} then ${exprToFormula(thenE)} else ${exprToFormula(elseE)}"

      case Matchless.Always(cond, result) =>
        s"(${boolExprToFormula(cond)}; ${exprToFormula(result)})"

      case Matchless.Lambda(_, recName, args, body) =>
        val argStr = args.toList.map(_.asString).mkString(", ")
        val rec = recName.fold("")(n => s"rec ${n.asString} ")
        s"$rec($argStr) => ${exprToFormula(body)}"

      case Matchless.MakeEnum(variant, arity, _) =>
        s"Enum($variant, arity=$arity)"

      case Matchless.MakeStruct(arity) =>
        s"Struct(arity=$arity)"

      case Matchless.ZeroNat =>
        "0"

      case Matchless.SuccNat =>
        "succ"

      case Matchless.PrevNat(n) =>
        s"pred(${exprToFormula(n)})"

      case Matchless.GetEnumElement(arg, _, idx, _) =>
        s"${exprToFormula(arg)}[$idx]"

      case Matchless.GetStructElement(arg, idx, _) =>
        s"${exprToFormula(arg)}.$idx"

      case Matchless.WhileExpr(cond, effect, result) =>
        s"while ${boolExprToFormula(cond)} do ${exprToFormula(effect)} return ${result}"
    }

  /**
   * Convert boolean expression to formula.
   */
  def boolExprToFormula[A](bexpr: Matchless.BoolExpr[A]): String =
    bexpr match {
      case Matchless.TrueConst => "true"
      case Matchless.EqualsLit(e, lit) =>
        val litStr = lit match {
          case Lit.Integer(i) => i.toString
          case Lit.Str(s) => s""""$s""""
          case Lit.Chr(c) => s"'$c'"
        }
        s"(${exprToFormula(e)} == $litStr)"
      case Matchless.EqualsNat(e, nat) =>
        val natStr = nat match {
          case dev.bosatsu.rankn.DataRepr.ZeroNat => "0"
          case dev.bosatsu.rankn.DataRepr.SuccNat => "> 0"
        }
        s"(${exprToFormula(e)} $natStr)"
      case Matchless.And(e1, e2) =>
        s"(${boolExprToFormula(e1)} && ${boolExprToFormula(e2)})"
      case Matchless.CheckVariant(e, expect, _, _) =>
        s"(${exprToFormula(e)}.tag == $expect)"
      case Matchless.SetMut(Matchless.LocalAnonMut(id), e) =>
        s"(_mut$id := ${exprToFormula(e)})"
      case Matchless.MatchString(arg, parts, _, _) =>
        s"(${exprToFormula(arg)} matches ${parts.mkString})"
      case Matchless.LetBool(Right(name), value, in) =>
        s"let ${name.asString} = ${exprToFormula(value)} in ${boolExprToFormula(in)}"
      case Matchless.LetBool(Left(Matchless.LocalAnon(id)), value, in) =>
        s"let _anon$id = ${exprToFormula(value)} in ${boolExprToFormula(in)}"
      case Matchless.LetMutBool(Matchless.LocalAnonMut(id), in) =>
        s"let mut _mut$id in ${boolExprToFormula(in)}"
    }

/**
   * Infer the value type from an expression.
   */
  def inferType[A](expr: Matchless.Expr[A]): String =
    expr match {
      case Matchless.Literal(lit) =>
        lit match {
          case _: Lit.Integer => "number"
          case _: Lit.Str => "string"
          case _: Lit.Chr => "string"
        }

      case Matchless.ZeroNat | Matchless.SuccNat | Matchless.PrevNat(_) =>
        "number"

      case Matchless.MakeEnum(_, 0, _) =>
        "boolean" // Zero-arity enums are often booleans

      case Matchless.App(Matchless.Global(_, PackageName.PredefName, name), _) =>
        name.asString match {
          case "add" | "sub" | "times" | "div" | "mod_Int" => "number"
          case "eq_Int" | "cmp_Int" => "boolean"
          case "concat_String" | "int_to_String" | "char_to_String" => "string"
          case _ => "any"
        }

      case _ => "any"
    }
}
