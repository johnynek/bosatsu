package org.bykn.bosatsu

import cats.evidence.Is
import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.{Type, ParsedTypeEnv}
import scala.collection.immutable.SortedSet

import Identifier.{Bindable, Constructor}

case class Program[T, D, S](types: T, lets: List[(Bindable, RecursionKind, D)], from: S) {
  private[this] lazy val letMap: Map[Bindable, (RecursionKind, D)] =
    lets.iterator.map { case (n, r, d) => (n, (r, d)) }.toMap

  def getLet(name: Bindable): Option[(RecursionKind, D)] = letMap.get(name)
  /**
   * main is the thing we evaluate. It is the last thing defined
   */
  def getMain[D1](fn: (Bindable, D1, D1) => D1)(implicit ev: D Is Expr[D1]): Option[Expr[D1]] = {
    @annotation.tailrec
    def loop(ls: List[(Bindable, RecursionKind, Expr[D1])], acc: Expr[D1]): Expr[D1] =
      ls match {
        case Nil => acc
        case (nm, rec, expr) :: tail =>
          val decl = fn(nm, expr.tag, acc.tag)
          loop(tail, Expr.Let(nm, expr, acc, recursive = rec, decl))
      }

    lets.reverse match {
      case (_, _, h) :: tail =>
        type L[X] = List[(Identifier.Bindable, RecursionKind, X)]
        Some(loop(ev.substitute[L](tail), ev.coerce(h)))
      case Nil =>
        None
    }
  }

  def getMainDecl(implicit ev: D Is Expr[Declaration]): Option[Expr[Declaration]] = {

    val fn = { (nm: Bindable, v: Declaration, in: Declaration) =>
      val r = v.region + in.region
      Declaration.Binding(BindingStatement(Pattern.Var(nm), v, Padding(0, in)))(r)
    }

    getMain[Declaration](fn)
  }
}

object Program {
  def fromStatement(
    pn0: PackageName,
    nameToType: Constructor => Type.Const,
    nameToCons: Constructor => (PackageName, Constructor),
    stmt: Statement): Program[ParsedTypeEnv[Unit], Expr[Declaration], Statement] = {

    import Statement._

    def declToE(d: Declaration): Expr[Declaration] =
      d.toExpr(nameToType, nameToCons)

    def defToT(
      types: ParsedTypeEnv[Unit],
      d: TypeDefinitionStatement): ParsedTypeEnv[Unit] =
      types.addDefinedType(d.toDefinition(pn0, nameToType))

    // Each time we need a name, we can call anonNames.next()
    // it is mutable, but in a limited scope
    val anonNames: Iterator[Bindable] = {
      val allNames =
        SortedSet(stmt.toStream.flatMap {
          case Bind(BindingStatement(bound, _, _)) => bound.names // TODO Keep identifiers
          case _ => Nil
        }: _*)

      rankn.Type
        .allBinders
        .iterator
        .map(_.name)
        .map(Identifier.Name(_))
        .filterNot(allNames)
    }

    def bindings(
      b: Pattern[(PackageName, Constructor), Type],
      decl: Expr[Declaration]): NonEmptyList[(Identifier.Bindable, Expr[Declaration])] =
      b match {
        case Pattern.Var(nm) =>
          NonEmptyList((nm, decl), Nil)
        case Pattern.Annotation(p, tpe) =>
          // we can just move the annotation to the expr:
          bindings(p, Expr.Annotation(decl, tpe, decl.tag))
        case complex =>
          val (prefix, rightHandSide) = decl match {
            case v@Expr.Var(_, _, _) =>
              // no need to make a new var to point to a var
              (Nil, v)
            case _ =>
              val ident = anonNames.next()
              val v = Expr.Var(None, ident, decl.tag)
              ((ident, decl) :: Nil, v)
          }

          val tail = complex.names.map { nm =>
            val pat = complex.filterVars(_ == nm)
            (nm, Expr.Match(rightHandSide,
              NonEmptyList.of((pat, Expr.Var(None, nm, decl.tag))), decl.tag))
          }

          def concat[A](ls: List[A], tail: NonEmptyList[A]): NonEmptyList[A] =
            ls match {
              case Nil => tail
              case h :: t => NonEmptyList(h, t ::: tail.toList)
            }

          NonEmptyList.fromList(tail) match {
            case Some(netail) =>
              concat(prefix, netail)
            case None =>
              // there are no names to bind here, but we still need to typecheck the match
              val dummy = anonNames.next()
              val pat = complex.unbind
              val shapeMatch = (dummy,
                Expr.Match(rightHandSide,
                  NonEmptyList.of((pat, Expr.Literal(Lit.fromInt(0), decl.tag))), decl.tag))
              concat(prefix, NonEmptyList(shapeMatch, Nil))
            }
      }

    def loop(s: Statement): Program[ParsedTypeEnv[Unit], Expr[Declaration], Statement] =
      s match {
        case Bind(BindingStatement(bound, decl, Padding(_, rest))) =>
          val Program(te, binds, _) = loop(rest)
          val pat = Declaration.unTuplePattern(bound, nameToType, nameToCons)
          val binds1 = bindings(pat, declToE(decl))
          val nonRec = binds1.toList.map { case (n, d) => (n, RecursionKind.NonRecursive, d) }
          Program(te, nonRec ::: binds, stmt)
        case Comment(CommentStatement(_, Padding(_, on))) =>
          loop(on).copy(from = s)
        case Def(defstmt@DefStatement(_, _, _, _)) =>
          val (lam, Program(te, binds, _)) = defstmt.result match {
            case (body, Padding(_, in)) =>
              // using body for the outer here is a bummer, but not really a good outer otherwise
              val l = defstmt.toLambdaExpr(declToE(body.get), body.get)(_.toType(nameToType))
              (l, loop(in))
          }
          Program(te, (defstmt.name, RecursionKind.Recursive, lam) :: binds, stmt)
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
          Program(ParsedTypeEnv.empty[Unit], Nil, EndOfFile)
      }

    loop(stmt)
  }

}
