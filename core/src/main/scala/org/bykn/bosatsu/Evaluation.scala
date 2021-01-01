package org.bykn.bosatsu

import cats.Eval
import org.bykn.bosatsu.rankn.Type
import scala.collection.mutable.{Map => MMap}

import cats.implicits._

import Identifier.Bindable

abstract class AbstractEvaluation[T, V] {
  val pm: PackageMap.Typed[T]
  val externals: Externals
  type F[A] = List[(Bindable, A)]
  def evaluateExpressions(
      exprs: F[Matchless.Expr],
      evalFn: (PackageName, Identifier) => Eval[V]
  ): F[Eval[V]]
  def wrapValue(value: Value): V
  def evalV(v: V): Value

  /**
   * Holds the final value of the environment for each Package
   */
  private[this] val envCache: MMap[PackageName, Map[Identifier, Eval[V]]] =
    MMap.empty

  private def externalEnv(p: Package.Typed[T]): Map[Identifier, Eval[Value]] = {
    val externalNames = p.program.externalDefs
    externalNames.iterator.map { n =>
      val tpe = p.program.types.getValue(p.name, n) match {
        case Some(t) => t
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown external def: $n")
          // $COVERAGE-ON$
      }
      externals.toMap.get((p.name, n.asString)) match {
        case Some(ext) => (n, Eval.later(ext.call(tpe)))
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} no External for external def: $n")
          // $COVERAGE-ON$
      }
    }
    .toMap
  }

  private[this] lazy val gdr = pm.getDataRepr

  private def evalLets(thisPack: PackageName, lets: List[(Bindable, RecursionKind, TypedExpr[T])]): List[(Bindable, Eval[V])] = {
    val exprs: List[(Bindable, Matchless.Expr)] =
      rankn.RefSpace
        .allocCounter
        .flatMap { c =>
          lets
            .traverse {
              case (name, rec, te) =>
                Matchless.fromLet(name, rec, te, gdr, c)
                 .map((name, _))
            }
      }
      .run
      .value

    val evalFn: (PackageName, Identifier) => Eval[V] =
      { (p, i) =>
        if (p == thisPack) Eval.defer(evaluate(p)(i))
        else evaluate(p)(i)
      }

    evaluateExpressions(exprs, evalFn)
  }

  private def evaluate(packName: PackageName): Map[Identifier, Eval[V]] =
    envCache.getOrElseUpdate(packName, {
      val pack = pm.toMap(packName)
      val extMap: Map[Identifier, Eval[V]] =
        externalEnv(pack).mapValues(_.map(wrapValue(_)))
      extMap ++ evalLets(packName, pack.program.lets)
    })

  def evaluateLast(p: PackageName): Option[(Eval[V], Type)] =
    for {
      pack <- pm.toMap.get(p)
      (name, _, tpe) <- pack.program.lets.lastOption
      value <- evaluate(p).get(name)
    } yield (value, tpe.getType)

  // TODO: this only works for lets, not externals
  def evaluateName(p: PackageName, name: Bindable): Option[(Eval[V], Type)] =
    for {
      pack <- pm.toMap.get(p)
      (_, _, tpe) <- pack.program.lets.filter { case (n, _, _) => n == name }.lastOption
      value <- evaluate(p).get(name)
    } yield (value, tpe.getType)

  /**
   * Return the last test, if any, in the package.
   * this is the test that is run when we test
   * the package
   */
  def lastTest(p: PackageName): Option[Eval[V]] =
    for {
      pack <- pm.toMap.get(p)
      (name, _, _) <- Package.testValue(pack)
      value <- evaluate(p).get(name)
    } yield value

  /* TODO: this is useful for debugging, but we should probably test it and write a parser for the
   * list syntax

  def repr: String = {
    val packs = pm.toMap.map { case (_, pack) =>
      Doc.text(s"(package ${pack.name.asString}") +
        (Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace,
          pack.program.lets.map { case (b, _, te) =>
            Doc.text(s"(let ${b.asString}") +
              (Doc.lineOrSpace + Doc.text(te.repr) + Doc.text(")")).nested(2)
          })).nested(2) + Doc.char(')')
    }

    Doc.intercalate(Doc.lineOrSpace, packs).render(80)
  }

  */

  def evalTest(ps: PackageName): Option[Eval[Test]] =
    lastTest(ps).map { ea =>
      ea.map(v => Test.fromValue(evalV(v)))
    }

  /**
   * Convert a typechecked value to Json
   * this code ASSUMES the type is correct. If not, we may throw or return
   * incorrect data.
   */
  val valueToJson: ValueToJson = ValueToJson({
    case Type.Const.Defined(pn, t) =>
      for {
        pack <- pm.toMap.get(pn)
        dt <- pack.program.types.getType(pn, t)
      } yield dt
  })

  /**
   * Convert a typechecked value to Doc
   * this code ASSUMES the type is correct. If not, we may throw or return
   * incorrect data.
   */
  val valueToDoc: ValueToDoc = ValueToDoc({
    case Type.Const.Defined(pn, t) =>
      for {
        pack <- pm.toMap.get(pn)
        dt <- pack.program.types.getType(pn, t)
      } yield dt
  })
}

case class Evaluation[T](pm: PackageMap.Typed[T], externals: Externals)
    extends AbstractEvaluation[T, Value] {
  def evalV(v: Value) = v
  def wrapValue(value: Value) = value

  private val ffunc = cats.Functor[List].compose(cats.Functor[(Bindable, *)])
  def evaluateExpressions(
      exprs: F[Matchless.Expr],
      evalFn: (PackageName, Identifier) => Eval[Value]
  ): F[Eval[Value]] = MatchlessToValue.traverse[F](exprs)(evalFn)(ffunc)
}

case class ExprEvaluation[T](pm: PackageMap.Typed[T], externals: Externals)
    extends AbstractEvaluation[T, MatchlessToValueWithExpr.ValueWithExpr] {
  def evalV(v: MatchlessToValueWithExpr.ValueWithExpr) = v.eval
  def wrapValue(value: Value) = MatchlessToValueWithExpr.ComputedValue(value)

  private val ffunc = cats.Functor[List].compose(cats.Functor[(Bindable, *)])
  def evaluateExpressions(
      exprs: F[Matchless.Expr],
      evalFn: (
          PackageName,
          Identifier
      ) => Eval[MatchlessToValueWithExpr.ValueWithExpr]
  ): F[Eval[MatchlessToValueWithExpr.ValueWithExpr]] =
    MatchlessToValueWithExpr.traverse[F](exprs)(evalFn)(ffunc)
}
