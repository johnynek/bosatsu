package dev.bosatsu.library

import cats.Eval
import cats.implicits._
import dev.bosatsu.hashing.Algo
import dev.bosatsu.rankn.{DefinedType, Type}
import dev.bosatsu.{Externals, Identifier, Matchless, MatchlessFromTypedExpr, MatchlessToValue, Package, PackageName, Par, Value, ValueToDoc, ValueToJson}
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedMap
import scala.collection.mutable.{Map => MMap}

import Identifier.Bindable

case class LibraryEvaluation(
    root: DecodedLibraryWithDeps[Algo.Blake3],
    externals: Externals
)(implicit ec: Par.EC) {
  type ScopeKey = (Name, Version)

  private def keyOrder: Ordering[ScopeKey] =
    Ordering.Tuple2(using summon[Ordering[Name]], summon[Ordering[Version]])

  private lazy val libsByKey: SortedMap[ScopeKey, DecodedLibraryWithDeps[
    Algo.Blake3
  ]] = {
    def loop(
        todo: List[DecodedLibraryWithDeps[Algo.Blake3]],
        acc: SortedMap[ScopeKey, DecodedLibraryWithDeps[
          Algo.Blake3
        ]]
    ): SortedMap[ScopeKey, DecodedLibraryWithDeps[
      Algo.Blake3
    ]] =
      todo match {
        case Nil         => acc
        case h :: tail   =>
          if (acc.contains(h.nameVersion)) loop(tail, acc)
          else loop(h.deps.values.toList ::: tail, acc.updated(h.nameVersion, h))
      }

    loop(root :: Nil, SortedMap.empty(using keyOrder))
  }

  private lazy val compiled: SortedMap[ScopeKey, MatchlessFromTypedExpr.Compiled[
    ScopeKey
  ]] =
    libsByKey.transform((_, dep) => dep.compile)

  private lazy val envCache: MMap[(ScopeKey, PackageName), Map[
    Identifier,
    Eval[Value]
  ]] =
    MMap.empty

  private def packageInScope(
      scope: ScopeKey,
      pack: PackageName
  ): Option[Package.Typed[Any]] =
    libsByKey
      .get(scope)
      .flatMap(_.lib.implementations.toMap.get(pack))

  private def depFor(src: ScopeKey, pn: PackageName): ScopeKey =
    libsByKey
      .get(src)
      .flatMap(_.depFor(pn))
      .map(_.nameVersion)
      .getOrElse(root.nameVersion)

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
      scope: ScopeKey,
      packName: PackageName,
      exprs: List[(Bindable, Matchless.Expr[ScopeKey])]
  ): List[(Bindable, Eval[Value])] = {
    val evalFn: (ScopeKey, PackageName, Identifier) => Eval[Value] =
      { (srcScope, p, i) =>
        val targetScope = depFor(srcScope, p)
        if (keyOrder.equiv(targetScope, scope) && p == packName)
          Eval.defer(evaluate(targetScope, p)(i))
        else evaluate(targetScope, p)(i)
      }

    type F[A] = List[(Bindable, A)]
    type BindablePair[A] = (Bindable, A)
    val ffunc: cats.Functor[F] =
      cats.Functor[List].compose[BindablePair](using cats.Functor[BindablePair])

    MatchlessToValue.traverse[F, ScopeKey](exprs)(evalFn)(using ffunc)
  }

  private def evaluate(
      scope: ScopeKey,
      packName: PackageName
  ): Map[Identifier, Eval[Value]] =
    envCache.getOrElseUpdate(
      (scope, packName), {
        val pack = packageInScope(scope, packName).getOrElse {
          // $COVERAGE-OFF$
          sys.error(s"missing package $packName in scope $scope")
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

  private lazy val packageCandidates: Map[PackageName, List[(ScopeKey, Package.Typed[Any])]] =
    libsByKey.iterator.flatMap { case (scope, dlwd) =>
      dlwd.lib.implementations.toMap.iterator.map { case (pn, pack) =>
        (pn, (scope, pack))
      }
    }.toList.groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  private def selectScopeFor(
      pn: PackageName
  ): Either[Throwable, ScopeKey] =
    packageCandidates.get(pn) match {
      case None | Some(Nil) =>
        Left(new Exception(s"package ${pn.asString} not found"))
      case Some((scope, _) :: Nil) =>
        Right(scope)
      case Some(scoped) =>
        val rootScope = root.nameVersion
        scoped.find { case (scope, _) => keyOrder.equiv(scope, rootScope) } match {
          case Some((scope, _)) => Right(scope)
          case None             =>
            val scopes = scoped.map(_._1).distinct.map { case (n, v) =>
              s"${n.name}:${v.render}"
            }
            Left(
              new Exception(
                s"package ${pn.asString} is ambiguous in dependency tree: ${scopes.mkString(", ")}"
              )
            )
        }
    }

  def evaluateMain(
      pn: PackageName
  ): Either[Throwable, (ScopeKey, Eval[Value], Type)] =
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

  def evaluateName(
      pn: PackageName,
      name: Bindable
  ): Either[Throwable, (ScopeKey, Eval[Value], Type)] =
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

  private def resolveDefinedType(
      rootScope: ScopeKey,
      const: Type.Const
  ): Option[DefinedType[Any]] =
    const match {
      case Type.Const.Defined(pn, t) =>
        val byScope = depFor(rootScope, pn)
        val inScope = packageInScope(byScope, pn).flatMap(_.types.getType(pn, t))
        inScope.orElse {
          packageCandidates
            .get(pn)
            .flatMap(_.headOption)
            .flatMap { case (_, pack) => pack.types.getType(pn, t) }
        }
    }

  def valueToJsonFor(scope: ScopeKey): ValueToJson =
    ValueToJson(resolveDefinedType(scope, _))

  def valueToDocFor(scope: ScopeKey): ValueToDoc =
    ValueToDoc(resolveDefinedType(scope, _))

  def packagesForShow(
      requested: List[PackageName]
  ): Either[Throwable, List[Package.Typed[Any]]] = {
    val rootPacks =
      libsByKey(root.nameVersion).lib.implementations.toMap.values.toList
    if (requested.isEmpty) Right(rootPacks.sortBy(_.name))
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
