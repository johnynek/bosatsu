package dev.bosatsu

import cats.data.NonEmptyList

object OperatorHints {
  private sealed abstract class LitKind
  private object LitKind {
    case object IntLit extends LitKind
    case object FloatLit extends LitKind
    case object StringLikeLit extends LitKind
  }

  private final case class CallSite(
      left: Option[LitKind],
      right: Option[LitKind]
  ) {
    def has(kind: LitKind): Boolean =
      left.contains(kind) || right.contains(kind)
  }

  def hint(
      op: Identifier.Operator,
      region: Region,
      lets: List[(Identifier.Bindable, RecursionKind, Expr[Declaration])]
  ): Option[String] = {
    val callSite =
      lets.iterator
        .map(_._3)
        .map(findCallSite(_, op, region))
        .collectFirst { case Some(site) => site }

    val predefFns = predefFunctions(op.asString, callSite)
    if (predefFns.isEmpty) None
    else {
      val opname = op.sourceCodeRepr
      Some(
        s"hint: `$opname` is often defined with ${formatPredefFns(predefFns)}. You can also define `$opname` locally."
      )
    }
  }

  private def findCallSite(
      expr: Expr[Declaration],
      op: Identifier.Operator,
      region: Region
  ): Option[CallSite] =
    expr match {
      case Expr.Annotation(inner, _, _) =>
        findCallSite(inner, op, region)
      case Expr.Local(_, _) | Expr.Global(_, _, _) | Expr.Literal(_, _) =>
        None
      case Expr.Generic(_, in) =>
        findCallSite(in, op, region)
      case Expr.Lambda(_, body, _) =>
        findCallSite(body, op, region)
      case Expr.App(fn, args, _) =>
        val atThisNode =
          (fn, args) match {
            case (
                  Expr.Local(opName: Identifier.Operator, tag),
                  NonEmptyList(left, right :: Nil)
                )
                if opName == op && regionContains(HasRegion.region(tag), region) =>
              Some(
                CallSite(
                  literalKind(left),
                  literalKind(right)
                )
              )
            case _ =>
              None
          }
        atThisNode
          .orElse(findCallSite(fn, op, region))
          .orElse(
            args.toList.iterator
              .map(findCallSite(_, op, region))
              .collectFirst { case Some(site) => site }
          )
      case Expr.Let(_, rhs, in, _, _) =>
        findCallSite(rhs, op, region).orElse(findCallSite(in, op, region))
      case Expr.Match(arg, branches, _) =>
        findCallSite(arg, op, region).orElse(
          branches.toList.iterator
            .map { br =>
              br.guard
                .flatMap(findCallSite(_, op, region))
                .orElse(findCallSite(br.expr, op, region))
            }
            .collectFirst { case Some(site) => site }
        )
    }

  private def literalKind(expr: Expr[Declaration]): Option[LitKind] =
    expr match {
      case Expr.Annotation(inner, _, _) =>
        literalKind(inner)
      case Expr.Literal(lit, _) =>
        lit match {
          case _: Lit.Integer => Some(LitKind.IntLit)
          case _: Lit.Float64 => Some(LitKind.FloatLit)
          case Lit.Str(_) | Lit.Chr(_) =>
            Some(LitKind.StringLikeLit)
        }
      case _ =>
        None
    }

  private def regionContains(outer: Region, inner: Region): Boolean =
    outer.start <= inner.start && inner.end <= outer.end

  private def hasIntLit(site: Option[CallSite]): Boolean =
    site.exists(_.has(LitKind.IntLit))

  private def hasFloatLit(site: Option[CallSite]): Boolean =
    site.exists(_.has(LitKind.FloatLit))

  private def hasStringLikeLit(site: Option[CallSite]): Boolean =
    site.exists(_.has(LitKind.StringLikeLit))

  private def numericFns(
      site: Option[CallSite],
      intFn: String,
      floatFn: String
  ): List[String] = {
    val intLit = hasIntLit(site)
    val floatLit = hasFloatLit(site)
    if (floatLit && !intLit) floatFn :: Nil
    else if (intLit && !floatLit) intFn :: Nil
    else intFn :: floatFn :: Nil
  }

  private def compareFns(site: Option[CallSite]): List[String] = {
    val intLit = hasIntLit(site)
    val floatLit = hasFloatLit(site)
    val strLit = hasStringLikeLit(site)

    if (strLit && !intLit && !floatLit) "cmp_String" :: Nil
    else if (floatLit && !intLit && !strLit) "cmp_Float64" :: Nil
    else if (intLit && !floatLit && !strLit) "cmp_Int" :: Nil
    else if (strLit || floatLit || intLit) {
      (if (intLit) "cmp_Int" :: Nil else Nil) :::
        (if (floatLit) "cmp_Float64" :: Nil else Nil) :::
        (if (strLit) "cmp_String" :: Nil else Nil)
    } else {
      "cmp_Int" :: "cmp_Float64" :: "cmp_String" :: Nil
    }
  }

  private def equalityFns(site: Option[CallSite]): List[String] = {
    val intLit = hasIntLit(site)
    val floatLit = hasFloatLit(site)
    val strLit = hasStringLikeLit(site)

    if (intLit && !floatLit && !strLit) "eq_Int" :: Nil
    else if (floatLit && !intLit && !strLit) "cmp_Float64" :: Nil
    else if (strLit && !intLit && !floatLit) "cmp_String" :: Nil
    else if (intLit || floatLit || strLit) {
      (if (intLit) "eq_Int" :: Nil else Nil) :::
        (if (floatLit) "cmp_Float64" :: Nil else Nil) :::
        (if (strLit) "cmp_String" :: Nil else Nil)
    } else {
      "eq_Int" :: "cmp_Int" :: "cmp_Float64" :: "cmp_String" :: Nil
    }
  }

  private def predefFunctions(
      rawOperator: String,
      site: Option[CallSite]
  ): List[String] =
    rawOperator match {
      case "+" =>
        numericFns(site, "add", "addf")
      case "-" =>
        numericFns(site, "sub", "subf")
      case "*" =>
        numericFns(site, "mul", "timesf")
      case "/" =>
        numericFns(site, "div", "divf")
      case "%" =>
        "mod_Int" :: Nil
      case "<<" =>
        "shift_left_Int" :: Nil
      case ">>" =>
        "shift_right_Int" :: Nil
      case "&" =>
        "and_Int" :: Nil
      case "|" =>
        "or_Int" :: Nil
      case "^" =>
        "xor_Int" :: Nil
      case "~" =>
        "not_Int" :: Nil
      case "==" =>
        equalityFns(site)
      case "<>" | "<" | "<=" | ">" | ">=" =>
        compareFns(site)
      case "++" =>
        if (hasStringLikeLit(site)) "concat_String" :: Nil
        else "concat" :: "concat_String" :: Nil
      case _ =>
        Nil
    }

  private def formatPredefFns(functions: List[String]): String = {
    val distinctFns = functions.distinct
    val joined =
      distinctFns match {
        case Nil           => ""
        case one :: Nil    => one
        case one :: two :: Nil =>
          s"$one or $two"
        case many =>
          val init = many.init.mkString(", ")
          s"$init, or ${many.last}"
      }
    s"Bosatsu/Predef::$joined"
  }
}
