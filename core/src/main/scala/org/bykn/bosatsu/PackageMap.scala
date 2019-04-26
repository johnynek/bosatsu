package org.bykn.bosatsu

import alleycats.std.map._ // TODO use SortedMap everywhere
import com.stripe.dagon.Memoize
import cats.{Foldable, Traverse}
import cats.data.{Chain, NonEmptyList, Validated, ValidatedNel, ReaderT, Writer}
import cats.Order
import cats.implicits._
import java.nio.file.Path
import org.typelevel.paiges.{Doc, Document}

import rankn.{Infer, Type, TypeEnv}

case class PackageMap[A, B, C, D](toMap: Map[PackageName, Package[A, B, C, D]]) {
  def +(pack: Package[A, B, C, D]): PackageMap[A, B, C, D] =
    PackageMap(toMap + (pack.name -> pack))

  def ++(packs: Iterable[Package[A, B, C, D]]): PackageMap[A, B, C, D] =
    packs.foldLeft(this)(_ + _)
}

object PackageMap {
  def empty[A, B, C, D]: PackageMap[A, B, C, D] =
    PackageMap(Map.empty)

  /**
   * Either return a unique mapping from B to A, or return a pair of duplicates
   * and the unque subset
   */
  def uniqueByKey[A, B: Order](as: List[A])(fn: A => B): Either[(Map[B, (A, NonEmptyList[A])], Map[B, A]), Map[B, A]] =
    as match {
      case Nil => Right(Map.empty)
      case h :: tail =>
        val nea = NonEmptyList(h, tail)
        val m: Map[B, NonEmptyList[A]] = nea.groupBy(fn)
        def check(as: NonEmptyList[A]): Either[(A, NonEmptyList[A]), A] =
          as match {
            case NonEmptyList(a, Nil) =>
              Right(a)
            case NonEmptyList(a0, a1 :: tail) =>
              Left((a0, NonEmptyList(a1, tail)))
          }

        val checked = m.map { case (b, as) => (b, check(as)) }
        val good = checked.collect { case (b, Right(a)) => (b, a) }.toMap
        val bad = checked.collect { case (b, Left(a)) => (b, a) }.toMap

        if (bad.isEmpty) Right(good)
        else Left((bad, good))
    }

  import Package.{ FixPackage, PackageF2 }

  type MapF3[A, B, C] = PackageMap[FixPackage[A, B, C], A, B, C]
  type MapF2[A, B] = MapF3[A, A, B]
  type ParsedImp = PackageMap[PackageName, Unit, Unit, (Statement, ImportMap[PackageName, Unit])]
  type Resolved = MapF2[Unit, (Statement, ImportMap[PackageName, Unit])]
  type Typed[T] = PackageMap[
    Package.Interface,
    NonEmptyList[Referant[Variance]],
    Referant[Variance],
    Program[
      TypeEnv[Variance],
      TypedExpr[T],
      Statement
    ]
  ]

  type Inferred = Typed[Declaration]

  /**
   * This builds a DAG of actual packages where names have been replaced by the fully resolved
   * packages
   */
  def resolvePackages[A, B, C](map: PackageMap[PackageName, A, B, C]): ValidatedNel[PackageError, MapF3[A, B, C]] = {
    def getPackage(i: Import[PackageName, A], from: Package[PackageName, A, B, C]): ValidatedNel[PackageError, Import[Package[PackageName, A, B, C], A]] =
      map.toMap.get(i.pack) match {
        case None => Validated.invalidNel(PackageError.UnknownImportPackage(i.pack, from))
        case Some(pack) => Validated.valid(Import(pack, i.items))
      }

    def step(p: Package[PackageName, A, B, C]): ReaderT[Either[NonEmptyList[PackageError], ?], List[PackageName], Package[FixPackage[A, B, C], A, B, C]] = {
      val edeps = ReaderT.ask[Either[NonEmptyList[PackageError], ?], List[PackageName]]
        .flatMapF {
          case nonE@(h :: tail) if nonE.contains(p.name) =>
            Left(NonEmptyList.of(PackageError.CircularDependency(p, NonEmptyList(h, tail))))
          case _ =>
            val deps = p.imports.traverse(getPackage(_, p)) // the packages p depends on
            deps.toEither
        }

      edeps
        .flatMap { deps =>
          deps.traverse { i =>
            step(i.pack)
              .local[List[PackageName]](p.name :: _) // add this package into the path of all the deps
              .map { p => Import(Fix[Lambda[a => Package[a, A, B, C]]](p), i.items) }
          }
          .map { imports =>
            Package(p.name, imports, p.exports, p.program)
          }
        }
    }

    type M = Map[PackageName, Package[FixPackage[A, B, C], A, B, C]]
    val r: ReaderT[Either[NonEmptyList[PackageError], ?], List[PackageName], M] =
      map.toMap.traverse(step)
    val m: Either[NonEmptyList[PackageError], M] = r.run(Nil)
    m.map(PackageMap(_)).toValidated
  }

  /**
   * Convenience method to create a PackageMap then resolve it
   */
  def resolveAll[A](ps: List[(A, Package.Parsed)]): (Map[PackageName, ((A, Package.Parsed), NonEmptyList[(A, Package.Parsed)])], ValidatedNel[PackageError, Resolved]) = {

    type AP = (A, Package.Parsed)
    val (nonUnique, unique): (Map[PackageName, (AP, NonEmptyList[AP])], Map[PackageName, AP]) =
      uniqueByKey(ps)(_._2.name) match {
        case Right(unique) =>
          (Map.empty, unique)
        case Left(res) => res
      }

    def toProg(p: Package.Parsed):
      (Option[PackageError],
        Package[PackageName, Unit, Unit, (Statement, ImportMap[PackageName, Unit])]) = {

      val (errs0, imap) = ImportMap.fromImports(p.imports)
      val errs = errs0 match {
        case Nil => None
        case h :: tail =>
          Some(PackageError.DuplicatedImport(NonEmptyList(h, tail)))
      }
      (errs, p.mapProgram((_, imap)))
    }

    // we know all the package names are unique here
    def foldMap(m: Map[PackageName, (A, Package.Parsed)]): (List[PackageError], PackageMap.ParsedImp) = {
      val initPm = PackageMap
        .empty[PackageName, Unit, Unit, (Statement, ImportMap[PackageName, Unit])]

      m.iterator.foldLeft((List.empty[PackageError], initPm)) {
        case ((errs, pm), (_, (_, pack))) =>
          val (lerrs, pp) = toProg(pack)
          (lerrs.toList ::: errs, pm + pp)
      }
    }

    val (errs, pmap) = foldMap(unique)
    val res = resolvePackages(pmap)
    // combine the import errors now:
    val check =
      errs match {
        case Nil =>
          Validated.valid(())
        case h :: tail =>
          Validated.invalid(NonEmptyList(h, tail))
      }
    // keep all the errors
    (nonUnique, check *> res)
  }

  /**
   * Infer all the types in a resolved PackageMap
   */
  def inferAll(ps: Resolved): ValidatedNel[PackageError, Inferred] = {

    // This is unfixed resolved
    type ResolvedU = PackageF2[Unit, (Statement, ImportMap[PackageName, Unit])]
    /*
     * We memoize this function to avoid recomputing diamond dependencies
     */
    val infer: ResolvedU => ValidatedNel[PackageError, Package.Inferred] =
      Memoize.function[ResolvedU, ValidatedNel[PackageError, Package.Inferred]] {
        // TODO, we ignore importMap here, we only check earlier we don't
        // have duplicate imports
        case (p@Package(nm, imports, exports, (stmt, importMap)), recurse) =>

          def getImport[A, B](packF: Package.Inferred,
            exMap: Map[Identifier, NonEmptyList[ExportedName[A]]],
            i: ImportedName[B]): ValidatedNel[PackageError, ImportedName[NonEmptyList[A]]] =
            exMap.get(i.originalName) match {
              case None =>
                Validated.invalidNel(
                  PackageError.UnknownImportName(
                    p, packF, i,
                    exMap.iterator.flatMap(_._2.toList).toList))
              case Some(exps) =>
                val bs = exps.map(_.tag)
                Validated.valid(i.map(_ => bs))
            }

          /*
           * This resolves imports from PackageNames into fully typed Packages
           *
           * Note the names are not unique after this step because an imported
           * type can have the same name as a constructor. After this step, each
           * distinct object has its own entry in the list
           */
          def stepImport(i: Import[Package.Resolved, Unit]):
            ValidatedNel[PackageError, Import[Package.Interface, NonEmptyList[Referant[Variance]]]] = {
            val Import(fixpack, items) = i
            recurse(fixpack.unfix).andThen { packF =>
              val packInterface = Package.interfaceOf(packF)
              val exMap = ExportedName.buildExportMap(packF.exports)
              items.traverse(getImport(packF, exMap, _))
                .map(Import(packInterface, _))
            }
          }

          imports
            .traverse(stepImport(_))
            .andThen { imps =>
              Package.inferBody(nm, imps, stmt)
                .map((imps, _))
            }
            .andThen { case (imps, (types, lets)) =>
              ExportedName.buildExports(nm, exports, types, lets)
                .map { exps =>
                  Package(nm, imps, exps, Program(types, lets, stmt))
                }
                .leftMap { badPackages =>
                  badPackages.map { n =>
                    PackageError.UnknownExport(n, p, lets)
                  }
                }
            }
        }

    ps.toMap.traverse(infer).map(PackageMap(_))
  }

  def resolveThenInfer[A](
    ps: List[(A, Package.Parsed)]): (Map[PackageName, ((A, Package.Parsed), NonEmptyList[(A, Package.Parsed)])], ValidatedNel[PackageError, Inferred]) = {
      val (bad, good) = resolveAll(ps)
      (bad, good.andThen(inferAll(_)))
    }

  def parseInputs[F[_]: Traverse](paths: F[Path]): ValidatedNel[Parser.Error, F[((Path, LocationMap), Package.Parsed)]] =
    paths.traverse { path =>
      Parser.parseFile(Package.parser, path).map { case (lm, parsed) =>
        ((path, lm), parsed)
      }
    }

  def buildSourceMap[F[_]: Foldable, A](parsedFiles: F[((A, LocationMap), Package.Parsed)]): Map[PackageName, (LocationMap, String)] =
    parsedFiles.foldLeft(Map.empty[PackageName, (LocationMap, String)]) { case (map, ((path, lm), pack)) =>
      map.updated(pack.name, (lm, path.toString))
    }
}

sealed abstract class PackageError {
  def message(sourceMap: Map[PackageName, (LocationMap, String)]): String
}

object PackageError {
  def showTypes(pack: PackageName, tpes: List[Type]): Map[Type, String] =
    TypeRef.fromTypes(Some(pack), tpes).map { case (k, v) =>
      (k, v.toDoc.render(80))
    }.toMap

  def nearest[A](ident: Identifier, existing: Map[Identifier, A], count: Int): List[(Identifier, A)] =
    existing
      .iterator
      .map { case (i, a) =>
        val d = EditDistance(ident.asString.toIterable, i.asString.toIterable)
        (i, d, a)
      }
      .filter(_._2 < ident.asString.length) // don't show things that require total edits
      .toList
      .sortBy { case (_, d, _) => d }
      .distinct
      .take(count)
      .map { case (i, _, a) => (i, a) }

  case class UnknownExport[A](ex: ExportedName[A],
    in: Package.PackageF2[Unit, (Statement, ImportMap[PackageName, Unit])],
    lets: List[(Identifier.Bindable, RecursionKind, TypedExpr[Declaration])]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = sourceMap(in.name)
      val header =
        s"in $sourceName unknown export ${ex.name}"
      val candidateMap: Map[Identifier, Region] =
        lets.map { case (n, _, expr) => (n, HasRegion.region(expr)) }.toMap
      val candidates =
        nearest(ex.name, candidateMap, 3)
          .map { case (n, r) =>
            val pos = lm.toLineCol(r.start).map { case (l, c) => s" at line: ${l + 1}, column: ${c + 1}" }.getOrElse("")
            s"${n.asString}$pos"
          }
      val candstr = candidates.mkString("\n\t", "\n\t", "\n")
      val suggestion =
        if (candidates.nonEmpty) "\n" + s"perhaps you meant:$candstr"
        else ""
      header + suggestion
    }
  }

  case class UnknownImportPackage[A, B, C](pack: PackageName, from: Package[PackageName, A, B, C]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (_, sourceName) = sourceMap(from.name)
      s"in $sourceName package ${from.name.asString} imports unknown package ${pack.asString}"
    }
  }

  case class DuplicatedImport(duplicates: NonEmptyList[(PackageName, ImportedName[Unit])]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) =
      duplicates.sortBy(_._2.localName)
        .toList
        .iterator
        .map { case (pack, imp) =>
          val (_, sourceName) = sourceMap(pack)
          s"duplicate import in $sourceName package ${pack.asString} imports ${imp.originalName} as ${imp.localName}"
        }
        .mkString("\n")
  }

  // We could check if we forgot to export the name in the package and give that error
  case class UnknownImportName[A, B](
    in: Package.PackageF2[Unit, (Statement, ImportMap[PackageName, Unit])],
    importing: Package.Inferred,
    iname: ImportedName[A],
    exports: List[ExportedName[B]]) extends PackageError {
      def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
        val ls = importing
          .program
          .lets

        val (_, sourceName) = sourceMap(in.name)
        val letMap = ls.iterator.map { case (n, _, _) => (n: Identifier, ()) }.toMap
        letMap
          .get(iname.originalName) match {
            case Some(_) =>
              s"in $sourceName package: ${importing.name} has ${iname.originalName} but it is not exported. Add to exports"
            case None =>
              val near = nearest(iname.originalName, letMap, 3)
                .map { case (n, _) => n.asString }
                .mkString(" Nearest: ", ", ", "")
              s"in $sourceName package: ${importing.name} does not have name ${iname.originalName}.$near"
          }
      }
    }
  case class CircularDependency[A, B, C](from: Package[PackageName, A, B, C], path: NonEmptyList[PackageName]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val packs = from.name :: (path.toList)
      val msg = packs.map { p =>
        val (_, src) = sourceMap(p)
        s"${p.asString} in $src"
      }
      val tab = "\n\t"
      s"circular package dependency:\n${msg.mkString(tab)}"
    }
  }

  case class CircularType[A](from: PackageName, path: NonEmptyList[rankn.DefinedType[A]]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      s"circular types in ${from.asString} " + path.toList.reverse.map(_.name.ident.asString).mkString(" -> ")
    }
  }

  case class VarianceInferenceFailure(from: PackageName, failed: NonEmptyList[rankn.DefinedType[Unit]]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      s"failed to infer variance in ${from.asString} of " + failed.toList.map(_.name.ident.asString).sorted.mkString(", ")
    }
  }

  case class TypeErrorIn(tpeErr: Infer.Error, pack: PackageName) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = sourceMap.get(pack) match {
        case None => (LocationMap(""), "<unknown source>")
        case Some(found) => found
      }

      val teMessage = tpeErr match {
        case Infer.Error.NotUnifiable(t0, t1, r0, r1) =>
          val context0 =
            if (r0 == r1) " " // sometimes the region of the error is the same on right and left
            else {
              val m = lm.showRegion(r0).getOrElse(r0.toString) // we should highlight the whole region
              s"\n$m\n"
            }
          val context1 =
            lm.showRegion(r1).getOrElse(r1.toString) // we should highlight the whole region

          val tmap = showTypes(pack, List(t0, t1))
          s"type ${tmap(t0)}${context0}does not unify with type ${tmap(t1)}\n$context1"
        case Infer.Error.VarNotInScope((pack, name), scope, region) =>
          val ctx = lm.showRegion(region).getOrElse(region.toString)
          val candidates: List[String] =
            nearest(name, scope.map { case ((_, n), _) => (n, ()) }, 3)
              .map { case (n, _) => n.asString }

          val cmessage =
            if (candidates.nonEmpty) candidates.mkString("\nClosest: ", ", ", ".\n")
            else ""
          val qname = "\"" + name + "\""
          s"name $qname unknown.$cmessage\n$ctx"
        case Infer.Error.SubsumptionCheckFailure(t0, t1, r0, r1, tvs) =>
          val context0 =
            if (r0 == r1) " " // sometimes the region of the error is the same on right and left
            else {
              val m = lm.showRegion(r0).getOrElse(r0.toString) // we should highlight the whole region
              s"\n$m\n"
            }
          val context1 =
            lm.showRegion(r1).getOrElse(r1.toString) // we should highlight the whole region

          val tmap = showTypes(pack, List(t0, t1))
          s"type ${tmap(t0)}${context0}does not subsume type ${tmap(t1)}\n$context1"
        case uc@Infer.Error.UnknownConstructor((p, n), region, _) =>
          val near = nearest(n, uc.knownConstructors.map { case (_, n) => (n, ()) }.toMap, 3)
            .map { case (n, _) => n.asString }

          val nearStr =
            if (near.isEmpty) ""
            else near.mkString(", nearest: ", ", ", "")

          val context =
            lm.showRegion(region).getOrElse(region.toString) // we should highlight the whole region

          s"unknown constructor ${n.asString}$nearStr" + "\n" + context
        case err => err.message
      }
      // TODO use the sourceMap/regiouns in Infer.Error
      s"in file: $sourceName, package ${pack.asString}, $teMessage"
    }
  }

  case class TotalityCheckError(pack: PackageName, err: TotalityCheck.ExprError[Declaration]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = sourceMap.get(pack) match {
        case None => (LocationMap(""), "<unknown source>")
        case Some(found) => found
      }
      val region = err.matchExpr.tag.region
      val context1 =
        lm.showRegion(region).getOrElse(region.toString) // we should highlight the whole region
      val teMessage = err match {
        case TotalityCheck.NonTotalMatch(_, missing) =>
          import Identifier.Constructor
          val allTypes = missing.traverse(_.traverseType { t => Writer(Chain.one(t), ()) })
            .run._1.toList.distinct
          val showT = showTypes(pack, allTypes)

          val doc = Pattern.compiledDocument(Document.instance[Type] { t =>
            Doc.text(showT(t))
          })
          def showPat(p: Pattern[(PackageName, Constructor), Type]): String =
            doc.document(p).render(80)

          "non-total match, missing: " +
            (Doc.intercalate(Doc.char(',') + Doc.lineOrSpace,
              missing.toList.map(doc.document(_)))).render(80)
        case TotalityCheck.InvalidPattern(_, err) =>
          import TotalityCheck._
          err match {
            case ArityMismatch((_, n), _, _, exp, found) =>
              s"arity mismatch: ${n.asString} expected $exp parameters, found $found"
            case UnknownConstructor((_, n), _, _) =>
              s"unknown constructor: ${n.asString}"
            case MultipleSplicesInPattern(_, _) =>
              "multiple splices in pattern, only one per match allowed"
          }
      }
      // TODO use the sourceMap/regions in Infer.Error
      s"in file: $sourceName, package ${pack.asString}\n$context1\n$teMessage"
    }
  }

  case class RecursionError(pack: PackageName, err: DefRecursionCheck.RecursionError) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = sourceMap(pack)
      val errMessage = err.toString
      // TODO use the sourceMap/regions in RecursionError
      s"in file: $sourceName, package ${pack.asString}, $errMessage"
    }
  }
}
