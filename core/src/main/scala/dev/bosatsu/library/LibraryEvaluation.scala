package dev.bosatsu.library

import cats.Eval
import cats.Functor
import cats.Order
import cats.implicits._
import dev.bosatsu.hashing.Algo
import dev.bosatsu.rankn.{DefinedType, Type}
import dev.bosatsu.{
  Externals,
  Identifier,
  Matchless,
  MatchlessFromTypedExpr,
  MatchlessToValue,
  Package,
  PackageMap,
  PackageName,
  Par,
  Test,
  Value,
  ValueToDoc,
  ValueToJson
}
import scala.collection.immutable.SortedMap
import scala.collection.mutable.{Map => MMap}

import Identifier.Bindable

case class LibraryEvaluation[K] private (
    rootScope: K,
    scopes: SortedMap[K, LibraryEvaluation.ScopeData[K]],
    renderScope: K => String,
    externals: Externals
)(implicit keyOrder: Ordering[K], ec: Par.EC) {
  given Order[K] = Order.fromOrdering(using keyOrder)

  private lazy val compiled: SortedMap[K, MatchlessFromTypedExpr.Compiled[K]] =
    scopes.transform { case (scope, data) =>
      MatchlessFromTypedExpr.compile(scope, data.packages)
    }

  private lazy val envCache: MMap[(K, PackageName), Map[Identifier, Eval[Value]]] =
    MMap.empty

  private def packageInScope(
      scope: K,
      pack: PackageName
  ): Option[Package.Typed[Any]] =
    scopes.get(scope).flatMap(_.packages.toMap.get(pack))

  private def depFor(src: K, pn: PackageName): K =
    scopes.get(src).flatMap(_.depForPackage(pn)).getOrElse(rootScope)

  private def externalEnv(
      pack: Package.Typed[Any]
  ): Map[Identifier, Eval[Value]] = {
    val externalNames = pack.externalDefs
    externalNames.iterator.map { n =>
      val tpe = pack.types.getValue(pack.name, n) match {
        case Some(t) => t
        case None    =>
          // $COVERAGE-OFF$
          sys.error(s"from ${pack.name} import unknown external def: $n")
        // $COVERAGE-ON$
      }
      externals.toMap.get((pack.name, n.asString)) match {
        case Some(ext) => (n, Eval.later(ext.call(tpe)))
        case None      =>
          // $COVERAGE-OFF$
          sys.error(s"from ${pack.name} no External for external def: $n")
        // $COVERAGE-ON$
      }
    }.toMap
  }

  private def evalLets(
      scope: K,
      packName: PackageName,
      exprs: List[(Bindable, Matchless.Expr[K])]
  ): List[(Bindable, Eval[Value])] = {
    val evalFn: (K, PackageName, Identifier) => Eval[Value] =
      { (srcScope, p, i) =>
        val targetScope = depFor(srcScope, p)
        if (keyOrder.equiv(targetScope, scope) && p == packName)
          Eval.defer(evaluate(targetScope, p)(i))
        else evaluate(targetScope, p)(i)
      }

    type F[A] = List[(Bindable, A)]
    type BindablePair[A] = (Bindable, A)
    val ffunc: Functor[F] =
      Functor[List].compose[BindablePair](using Functor[BindablePair])

    MatchlessToValue.traverse[F, K](exprs)(evalFn)(using ffunc)
  }

  private def evaluate(
      scope: K,
      packName: PackageName
  ): Map[Identifier, Eval[Value]] =
    envCache.getOrElseUpdate(
      (scope, packName), {
        val pack = packageInScope(scope, packName).getOrElse {
          // $COVERAGE-OFF$
          sys.error(
            s"missing package $packName in scope ${renderScope(scope)}"
          )
        // $COVERAGE-ON$
        }
        val lets =
          compiled
            .get(scope)
            .flatMap(_.get(packName))
            .getOrElse(Nil)
        externalEnv(pack) ++ evalLets(scope, packName, lets)
      }
    )

  private lazy val packageCandidates: Map[PackageName, List[(K, Package.Typed[Any])]] =
    scopes.iterator.flatMap { case (scope, data) =>
      data.packages.toMap.iterator.map { case (pn, pack) =>
        (pn, (scope, pack))
      }
    }.toList.groupMap(_._1)(_._2)

  private def selectScopeFor(
      pn: PackageName
  ): Either[Throwable, K] =
    packageCandidates.get(pn) match {
      case None | Some(Nil) =>
        Left(new Exception(s"package ${pn.asString} not found"))
      case Some((scope, _) :: Nil) =>
        Right(scope)
      case Some(scoped) =>
        scoped.find { case (scope, _) => keyOrder.equiv(scope, rootScope) } match {
          case Some((scope, _)) => Right(scope)
          case None             =>
            val scopeStrings = scoped.map(_._1).distinct.map(renderScope(_))
            Left(
              new Exception(
                s"package ${pn.asString} is ambiguous in dependency tree: ${scopeStrings.mkString(", ")}"
              )
            )
        }
    }

  def evaluateMain(
      pn: PackageName
  ): Either[Throwable, (K, Eval[Value], Type)] =
    for {
      scope <- selectScopeFor(pn)
      pack <- packageInScope(scope, pn).toRight(
        new Exception(s"package ${pn.asString} not found in scope")
      )
      (name, _, te) <- Package.mainValue(pack).toRight(
        new Exception(s"found no main expression in package ${pn.asString}")
      )
      value <- evaluate(scope, pn).get(name).toRight(
        new Exception(s"could not evaluate ${pn.asString}::${name.asString}")
      )
    } yield (scope, value, te.getType)

  def evaluateMainValue(
      pn: PackageName
  ): Either[Throwable, (Eval[Value], Type)] =
    evaluateMain(pn).map { case (_, value, tpe) =>
      (value, tpe)
    }

  def evaluateName(
      pn: PackageName,
      name: Bindable
  ): Either[Throwable, (K, Eval[Value], Type)] =
    for {
      scope <- selectScopeFor(pn)
      pack <- packageInScope(scope, pn).toRight(
        new Exception(s"package ${pn.asString} not found in scope")
      )
      (_, _, te) <- pack.lets.findLast(_._1 == name).toRight(
        new Exception(s"value ${pn.asString}::${name.asString} not found")
      )
      value <- evaluate(scope, pn).get(name).toRight(
        new Exception(s"could not evaluate ${pn.asString}::${name.asString}")
      )
    } yield (scope, value, te.getType)

  def evaluateNameValue(
      pn: PackageName,
      name: Bindable
  ): Either[Throwable, (Eval[Value], Type)] =
    evaluateName(pn, name).map { case (_, value, tpe) =>
      (value, tpe)
    }

  def evalTest(pn: PackageName): Option[Eval[Test]] =
    selectScopeFor(pn).toOption.flatMap { scope =>
      for {
        pack <- packageInScope(scope, pn)
        (name, _, _) <- Package.testValue(pack)
        value <- evaluate(scope, pn).get(name)
      } yield value.map(Test.fromValue(_))
    }

  private def resolveDefinedType(
      scope: K,
      const: Type.Const
  ): Option[DefinedType[Any]] =
    const match {
      case Type.Const.Defined(pn, t) =>
        val byScope = depFor(scope, pn)
        val inScope = packageInScope(byScope, pn).flatMap(_.types.getType(pn, t))
        inScope.orElse {
          packageCandidates
            .get(pn)
            .flatMap(_.headOption)
            .flatMap { case (_, pack) => pack.types.getType(pn, t) }
        }
    }

  def valueToJsonFor(scope: K): ValueToJson =
    ValueToJson(resolveDefinedType(scope, _))

  def valueToDocFor(scope: K): ValueToDoc =
    ValueToDoc(resolveDefinedType(scope, _))

  def valueToJson: ValueToJson =
    valueToJsonFor(rootScope)

  def valueToDoc: ValueToDoc =
    valueToDocFor(rootScope)

  def packagesForShow(
      requested: List[PackageName]
  ): Either[Throwable, List[Package.Typed[Any]]] = {
    scopes.get(rootScope).toRight(new Exception("missing root scope")).flatMap {
      rootData =>
        if (requested.isEmpty) Right(rootData.packages.toMap.values.toList.sortBy(_.name))
        else
          requested.traverse { pn =>
            for {
              scope <- selectScopeFor(pn)
              pack <- packageInScope(scope, pn).toRight(
                new Exception(s"package ${pn.asString} not found")
              )
            } yield pack
          }
    }
  }
}

object LibraryEvaluation {
  final case class ScopeData[K](
      packages: PackageMap.Typed[Any],
      depForPackage: PackageName => Option[K]
  )

  def fromPackageMap[T](
      pm: PackageMap.Typed[T],
      externals: Externals
  )(implicit ec: Par.EC): LibraryEvaluation[Unit] = {
    val data =
      ScopeData[Unit](
        packages = PackageMap.toAnyTyped(pm),
        depForPackage = _ => Some(())
      )

    new LibraryEvaluation[Unit](
      rootScope = (),
      scopes = SortedMap(() -> data)(using Ordering.Unit),
      renderScope = _ => "root",
      externals = externals
    )(using Ordering.Unit, ec)
  }

  def apply(
      root: DecodedLibraryWithDeps[Algo.Blake3],
      externals: Externals
  )(implicit ec: Par.EC): LibraryEvaluation[(Name, Version)] = {
    type ScopeKey = (Name, Version)
    given Ordering[ScopeKey] = Ordering.Tuple2

    def loop(
        todo: List[DecodedLibraryWithDeps[Algo.Blake3]],
        acc: SortedMap[ScopeKey, ScopeData[ScopeKey]]
    ): SortedMap[ScopeKey, ScopeData[ScopeKey]] =
      todo match {
        case Nil => acc
        case head :: tail =>
          if (acc.contains(head.nameVersion)) loop(tail, acc)
          else {
            val data = ScopeData(
              packages = head.lib.implementations,
              depForPackage = pn => head.depFor(pn).map(_.nameVersion)
            )
            loop(head.deps.values.toList ::: tail, acc.updated(head.nameVersion, data))
          }
      }

    val scopes =
      loop(root :: Nil, SortedMap.empty[ScopeKey, ScopeData[ScopeKey]])

    new LibraryEvaluation[ScopeKey](
      rootScope = root.nameVersion,
      scopes = scopes,
      renderScope = { case (name, version) =>
        s"${name.name}:${version.render}"
      },
      externals = externals
    )
  }
}
