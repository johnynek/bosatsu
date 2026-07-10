package dev.bosatsu

import cats.Eval
import dev.bosatsu.rankn.Type
import scala.collection.mutable.{Map => MMap}

import cats.implicits._

import Identifier.Bindable

case class Evaluation[T](pm: PackageMap.Typed[T], externals: Externals) {

  /** Holds the final value of the environment for each Package
    */
  private val envCache: MMap[PackageName, Map[Identifier, Eval[Value]]] =
    MMap.empty

  private def externalEnv(p: Package.Typed[T]): Map[Identifier, Eval[Value]] = {
    val externalNames = p.externalDefs
    externalNames.iterator.map { n =>
      val tpe = p.types.getValue(p.name, n) match {
        case Some(t) => t
        case None    =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown external def: $n")
        // $COVERAGE-ON$
      }
      externals.toMap.get((p.name, n)) match {
        case Some(ext) => (n, Eval.later(ext.call(tpe)))
        case None      =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} no External for external def: $n")
        // $COVERAGE-ON$
      }
    }.toMap
  }

  private lazy val gdr = pm.getDataRepr

  private def evalLets(
      thisPack: PackageName,
      lets: List[(Bindable, RecursionKind, TypedExpr[T])]
  ): List[(Bindable, Eval[Value])] = {
    val exprs: List[(Bindable, Matchless.Expr[Unit])] =
      rankn.RefSpace.allocCounter
        .flatMap { c =>
          lets
            .traverse { case (name, rec, te) =>
              Matchless
                .fromLet((), name, rec, te, gdr, c)
                .map((name, _))
            }
        }
        .run
        .value

    val evalFn: (Unit, PackageName, Identifier) => Eval[Value] = { (_, p, i) =>
      if (p == thisPack) Eval.defer(evaluate(p)(i))
      else evaluate(p)(i)
    }

    type F[A] = List[(Bindable, A)]
    type BindablePair[A] = (Bindable, A)
    val ffunc: cats.Functor[F] =
      cats.Functor[List].compose[BindablePair](using cats.Functor[BindablePair])
    MatchlessToValue.traverse[F, Unit](exprs)(evalFn)(using ffunc)
  }

  private def evaluate(packName: PackageName): Map[Identifier, Eval[Value]] =
    envCache.getOrElseUpdate(
      packName, {
        val pack = pm.toMap(packName)
        externalEnv(pack) ++ evalLets(packName, pack.lets)
      }
    )

  // TODO: this only works for lets, not externals (https://github.com/johnynek/bosatsu/issues/186)
  def evaluateName(
      p: PackageName,
      name: Bindable
  ): Option[(Eval[Value], Type)] =
    for {
      pack <- pm.toMap.get(p)
      (_, _, tpe) <- pack.lets.filter { case (n, _, _) =>
        n == name
      }.lastOption
      value <- evaluate(p).get(name)
    } yield (value, tpe.getType)

  private def testEntryValue(
      p: PackageName
  ): Either[Package.TestDiscoveryError, Option[
    (
        Package.TestEntry[T],
        Eval[
          Value
        ]
    )
  ]] =
    pm.toMap.get(p) match {
      case None       => Right(None)
      case Some(pack) =>
        Package
          .testEntry(pack)
          .map(_.flatMap { entry =>
            evaluate(p).get(entry.bindable).map((entry, _))
          })
    }

  def evaluateMain(p: PackageName): Option[(Eval[Value], Type)] =
    for {
      pack <- pm.toMap.get(p)
      (name, _, te) <- Package.mainValue(pack)
      value <- evaluate(p).get(name)
    } yield (value, te.getType)

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

  def evalTest(
      ps: PackageName
  ): Either[Package.TestDiscoveryError, Option[Eval[Test]]] =
    testEntryValue(ps).map(
      _.map { case (entry, evalValue) =>
        entry match {
          case Package.TestEntry.PlainTest(_, _, _) =>
            evalValue.map(Test.fromValue(_))
          case progTest @ Package.TestEntry.ProgTest(_, _, _) =>
            evalValue.map { value =>
              PredefImpl.runProgTest(value, Nil) match {
                case Right(testValue) =>
                  Test.fromValue(testValue)
                case Left(errValue) =>
                  Test.Assertion(
                    false,
                    s"ProgTest ${ps.asString}::${progTest.bindable.sourceCodeRepr} raised an uncaught error: $errValue"
                  )
              }
            }
        }
      }
    )

  /** Convert a typechecked value to Json this code ASSUMES the type is correct.
    * If not, we may throw or return incorrect data.
    */
  val valueToJson: ValueToJson = ValueToJson { case Type.Const.Defined(pn, t) =>
    for {
      pack <- pm.toMap.get(pn)
      dt <- pack.types.getType(pn, t)
    } yield dt
  }

  /** Convert a typechecked value to Doc this code ASSUMES the type is correct.
    * If not, we may throw or return incorrect data.
    */
  val valueToDoc: ValueToDoc = ValueToDoc { case Type.Const.Defined(pn, t) =>
    for {
      pack <- pm.toMap.get(pn)
      dt <- pack.types.getType(pn, t)
    } yield dt
  }
}
