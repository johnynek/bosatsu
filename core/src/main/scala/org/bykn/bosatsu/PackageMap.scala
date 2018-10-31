package org.bykn.bosatsu

import alleycats.std.map._ // TODO use SortedMap everywhere
import com.stripe.dagon.Memoize
import cats.data.{NonEmptyList, Validated, ValidatedNel, ReaderT}
import cats.Order
import cats.implicits._

import rankn.Infer

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

  import Package.{ FixPackage, PackageF2, PackageF }

  type MapF3[A, B, C] = PackageMap[FixPackage[A, B, C], A, B, C]
  type MapF2[A, B] = MapF3[A, A, B]
  type ParsedImp = PackageMap[PackageName, Unit, Unit, (Statement, ImportMap[PackageName, Unit])]
  type Resolved = MapF2[Unit, (Statement, ImportMap[PackageName, Unit])]
  type Inferred = MapF3[NonEmptyList[Referant], Referant, Program[TypedExpr[Declaration], Statement]]

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

    type PackIn = PackageF2[Unit, (Statement, ImportMap[PackageName, Unit])]
    type PackOut = PackageF[NonEmptyList[Referant], Referant, Program[TypedExpr[Declaration], Statement]]

    /*
     * We memoize this function to avoid recomputing diamond dependencies
     */
    val infer: PackIn => ValidatedNel[PackageError, PackOut] =
      Memoize.function[PackIn, ValidatedNel[PackageError, PackOut]] {
        // TODO, we ignore importMap here, we only check earlier we don't
        // have duplicate imports
        case (p@Package(nm, imports, exports, (stmt, importMap)), recurse) =>

          def getImport[A, B](packF: PackOut,
            exMap: Map[String, NonEmptyList[ExportedName[A]]],
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
            ValidatedNel[PackageError, Import[Package.Inferred, NonEmptyList[Referant]]] = {
            val Import(fixpack, items) = i
            recurse(fixpack.unfix).andThen { packF =>
              val exMap = ExportedName.buildExportMap(packF.exports)
              items.traverse(getImport(packF, exMap, _))
                .map { imps =>
                  Import(
                    Fix[Lambda[a =>
                          Package[a,
                            NonEmptyList[Referant],
                            Referant,
                            Program[TypedExpr[Declaration], Statement]]]](
                      packF),
                    imps)
                }
            }
          }

          imports
            .traverse(stepImport(_))
            .andThen { imps =>
              Package.inferBody(nm, imps, stmt) match {
                case Left(err) =>
                  Validated.invalidNel(err) // TODO give better errors
                case Right(good) =>
                  Validated.valid((imps, good))
              }
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
}

sealed abstract class PackageError {
  def message(sourceMap: Map[PackageName, (LocationMap, String)]): String
}

object PackageError {
  case class UnknownExport[A](ex: ExportedName[A],
    in: Package.PackageF2[Unit, (Statement, ImportMap[PackageName, Unit])],
    lets: List[(String, TypedExpr[Declaration])]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = sourceMap(in.name)
      val header =
        s"in $sourceName unknown export ${ex.name}"
      val candidates = lets
        .map { case (n, expr) => (EditDistance.string(n, ex.name), n, HasRegion.region(expr)) }
        .sorted
        .take(3)
        .map { case (_, n, r) =>
          val pos = lm.toLineCol(r.start).map { case (l, c) => s" at line: ${l + 1}, column: ${c + 1}" }.getOrElse("")
          s"$n$pos"
        }
      val candstr = candidates.mkString("\n\t", "\n\t", "\n")
      val suggestion = s"perhaps you meant:$candstr"
      header + "\n" + suggestion
    }
  }

  case class UnknownImportPackage[A, B, C](pack: PackageName, from: Package[PackageName, A, B, C]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (_, sourceName) = sourceMap(pack)
      s"in $sourceName package ${pack.asString} imports unknown package ${from.name.asString}"
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
    importing: Package.PackageF[NonEmptyList[Referant], Referant, Program[TypedExpr[Declaration], Statement]],
    iname: ImportedName[A],
    exports: List[ExportedName[B]]) extends PackageError {
      def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
        val ls = importing
          .program
          .lets

        val (_, sourceName) = sourceMap(in.name)
        ls
          .toMap
          .get(iname.originalName) match {
            case Some(_) =>
              s"in $sourceName package: ${importing.name} has ${iname.originalName} but it is not exported. Add to exports"
            case None =>
              val dist = Memoize.function[String, Int] { (s, _) => EditDistance(iname.originalName.toIterable, s.toIterable) }
              val nearest = ls.map { case (n, _) => (dist(n), n) }.sorted.take(3).map { case (_, n) => n }.mkString(", ")
              s"in $sourceName package: ${importing.name} does not have name ${iname.originalName}. Nearest: $nearest"
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

  case class CircularType(from: PackageName, path: NonEmptyList[rankn.DefinedType]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      s"circular types in ${from.asString}" + path.toList.reverse.map(_.name.asString).mkString(" -> ")
    }
  }

  case class TypeErrorIn(tpeErr: Infer.Error, pack: PackageName) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = sourceMap(pack)
      val teMessage = tpeErr.message
      // TODO use the sourceMap/regiouns in Infer.Error
      s"in file: $sourceName, package ${pack.asString}, $teMessage"
    }
  }
}
