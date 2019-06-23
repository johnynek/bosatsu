package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.{Type, ParsedTypeEnv}
import scala.collection.immutable.SortedSet

import Identifier.{Bindable, Constructor}

case class Program[T, +D, +S](
  types: T,
  lets: List[(Bindable, RecursionKind, D)],
  externalDefs: List[Bindable],
  from: S) {

  private[this] lazy val letMap: Map[Bindable, (RecursionKind, D)] =
    lets.iterator.map { case (n, r, d) => (n, (r, d)) }.toMap

  def getLet(name: Bindable): Option[(RecursionKind, D)] = letMap.get(name)
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
          val Program(te, binds, exts, _) = loop(rest)
          val pat = Declaration.unTuplePattern(bound, nameToType, nameToCons)
          val binds1 = bindings(pat, declToE(decl))
          val nonRec = binds1.toList.map { case (n, d) => (n, RecursionKind.NonRecursive, d) }
          Program(te, nonRec ::: binds, exts, stmt)
        case Comment(CommentStatement(_, Padding(_, on))) =>
          loop(on).copy(from = s)
        case Def(defstmt@DefStatement(_, _, _, _)) =>
          // using body for the outer here is a bummer, but not really a good outer otherwise
          val lam = defstmt.toLambdaExpr({ res => declToE(res._1.get) },
            defstmt.result._1.get)(Declaration.unTuplePattern(_, nameToType, nameToCons), _.toType(nameToType))

          val Program(te, binds, exts, _) = defstmt.result match {
            case (_, Padding(_, in)) => loop(in)
          }
          Program(te, (defstmt.name, RecursionKind.Recursive, lam) :: binds, exts, stmt)
        case s@Struct(_, _, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = defToT(p.types, s), from = s)
        case e@Enum(_, _, _, Padding(_, rest)) =>
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
          p.copy(
            types = p.types.addExternalValue(pn0, name, maybeForAll),
            externalDefs = name :: p.externalDefs,
            from = s)
        case x@ExternalStruct(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = defToT(p.types, x), from = x)
        case EndOfFile =>
          Program(ParsedTypeEnv.empty[Unit], Nil, Nil, EndOfFile)
      }

    loop(stmt)
  }
}
