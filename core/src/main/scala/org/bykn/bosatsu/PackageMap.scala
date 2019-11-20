package org.bykn.bosatsu

import alleycats.std.map._ // TODO use SortedMap everywhere
import org.bykn.bosatsu.graph.Memoize
import cats.{Apply, Applicative, Foldable}
import cats.arrow.FunctionK
import cats.data.{NonEmptyList, Validated, ValidatedNel, ReaderT}
import cats.Order
import cats.implicits._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

import rankn.TypeEnv

case class PackageMap[A, B, C, +D](toMap: Map[PackageName, Package[A, B, C, D]]) {
  def +[D1 >: D](pack: Package[A, B, C, D1]): PackageMap[A, B, C, D1] =
    PackageMap(toMap + (pack.name -> pack))

  def ++[D1 >: D](packs: Iterable[Package[A, B, C, D1]]): PackageMap[A, B, C, D1] =
    packs.foldLeft(this: PackageMap[A, B, C, D1])(_ + _)
}

object PackageMap {
  def empty[A, B, C, D]: PackageMap[A, B, C, D] =
    PackageMap(Map.empty)

  def fromIterable[A, B, C, D](ps: Iterable[Package[A, B, C, D]]): PackageMap[A, B, C, D] =
    empty[A, B, C, D] ++ ps
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

  import Package.FixPackage

  type MapF3[A, B, C] = PackageMap[FixPackage[A, B, C], A, B, C]
  type MapF2[A, B] = MapF3[A, A, B]
  type ParsedImp = PackageMap[PackageName, Unit, Unit, (List[Statement], ImportMap[PackageName, Unit])]
  type Resolved = MapF2[Unit, (List[Statement], ImportMap[PackageName, Unit])]
  type Typed[+T] = PackageMap[
    Package.Interface,
    NonEmptyList[Referant[Variance]],
    Referant[Variance],
    Program[
      TypeEnv[Variance],
      TypedExpr[T],
      Any
    ]
  ]

  // convenience for type inference
  def toAnyTyped[A](p: Typed[A]): Typed[Any] = p

  type Inferred = Typed[Declaration]

  /**
   * This builds a DAG of actual packages where names have been replaced by the fully resolved
   * packages
   */
  def resolvePackages[A, B, C](map: PackageMap[PackageName, A, B, C], ifs: List[Package.Interface]): ValidatedNel[PackageError, MapF3[A, B, C]] = {
    val interfaceMap = ifs.iterator.map { iface => (iface.name, iface) }.toMap

    def getPackage(
      i: Import[PackageName, A],
      from: Package[PackageName, A, B, C]): ValidatedNel[PackageError, Import[Either[Package.Interface, Package[PackageName, A, B, C]], A]] =
        map.toMap.get(i.pack) match {
          case Some(pack) => Validated.valid(Import(Right(pack), i.items))
          case None =>
            interfaceMap.get(i.pack) match {
              case Some(iface) =>
                Validated.valid(Import(Left(iface), i.items))
              case None =>
                Validated.invalidNel(PackageError.UnknownImportPackage(i.pack, from))
            }
        }

    type PackageFix = Package[FixPackage[A, B, C], A, B, C]
    // We use the ReaderT to build the list of imports we are on
    // to detect circular dependencies, if the current package imports itself transitively we
    // want to report the full path
    val step: Package[PackageName, A, B, C] => ReaderT[Either[NonEmptyList[PackageError], ?], List[PackageName], PackageFix] =
      Memoize.memoizeDagHashed[Package[PackageName, A, B, C], ReaderT[Either[NonEmptyList[PackageError], ?], List[PackageName], PackageFix]] { (p, rec) =>
      val edeps = ReaderT.ask[Either[NonEmptyList[PackageError], ?], List[PackageName]]
        .flatMapF {
          case nonE@(h :: tail) if nonE.contains(p.name) =>
            Left(NonEmptyList.of(PackageError.CircularDependency(p, NonEmptyList(h, tail))))
          case _ =>
            val deps = p.imports.traverse(getPackage(_, p)) // the packages p depends on
            deps.toEither
        }

      edeps
        .flatMap { deps: List[Import[Either[Package.Interface, Package[PackageName, A, B, C]], A]] =>
          deps.traverse { i =>
            i.pack match {
              case Right(pack) =>
                rec(pack)
                  .local[List[PackageName]](p.name :: _) // add this package into the path of all the deps
                  .map { p => Import(Package.fix[A, B, C](Right(p)), i.items) }
              case Left(iface) =>
                ReaderT.pure[
                  Either[NonEmptyList[PackageError], ?],
                  List[PackageName],
                  Import[FixPackage[A, B, C], A]](Import(Package.fix[A, B, C](Left(iface)), i.items))
            }
          }
          .map { imports =>
            Package(p.name, imports, p.exports, p.program)
          }
        }
    }

    type M = Map[PackageName, PackageFix]
    val r: ReaderT[Either[NonEmptyList[PackageError], ?], List[PackageName], M] =
      map.toMap.traverse(step)

    // we start with no imports on
    val m: Either[NonEmptyList[PackageError], M] = r.run(Nil)

    m.map(PackageMap(_)).toValidated
  }

  /**
   * Convenience method to create a PackageMap then resolve it
   */
  def resolveAll[A](ps: List[(A, Package.Parsed)], ifs: List[Package.Interface]): (Map[PackageName, ((A, Package.Parsed), NonEmptyList[(A, Package.Parsed)])], ValidatedNel[PackageError, Resolved]) = {

    type AP = (A, Package.Parsed)
    val (nonUnique, unique): (Map[PackageName, (AP, NonEmptyList[AP])], Map[PackageName, AP]) =
      uniqueByKey(ps)(_._2.name) match {
        case Right(unique) =>
          (Map.empty, unique)
        case Left(res) => res
      }

    def toProg(p: Package.Parsed):
      (Option[PackageError],
        Package[PackageName, Unit, Unit, (List[Statement], ImportMap[PackageName, Unit])]) = {

      val (errs0, imap) = ImportMap.fromImports(p.imports)
      val errs =
        NonEmptyList.fromList(errs0)
          .map(PackageError.DuplicatedImport)

      (errs, p.mapProgram((_, imap)))
    }

    // we know all the package names are unique here
    def foldMap(m: Map[PackageName, (A, Package.Parsed)]): (List[PackageError], PackageMap.ParsedImp) = {
      val initPm = PackageMap
        .empty[PackageName, Unit, Unit, (List[Statement], ImportMap[PackageName, Unit])]

      m.iterator.foldLeft((List.empty[PackageError], initPm)) {
        case ((errs, pm), (_, (_, pack))) =>
          val (lerrs, pp) = toProg(pack)
          (lerrs.toList ::: errs, pm + pp)
      }
    }

    val (errs, pmap) = foldMap(unique)
    val res = resolvePackages(pmap, ifs)
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
  def inferAll(ps: Resolved)(implicit cpuEC: ExecutionContext): ValidatedNel[PackageError, Inferred] = {

    // This is unfixed resolved
    type ResolvedU = Package[
      FixPackage[Unit, Unit, (List[Statement], ImportMap[PackageName, Unit])],
      Unit,
      Unit,
      (List[Statement], ImportMap[PackageName, Unit])]

    type FutVal[+A] = Future[ValidatedNel[PackageError, A]]
    val futValid: Applicative[FutVal] = Applicative[Future].compose[ValidatedNel[PackageError, ?]]
    /*
     * We memoize this function to avoid recomputing diamond dependencies
     */
    val infer: ResolvedU => FutVal[Package.Inferred] =
      Memoize.memoizeDagFuture[ResolvedU, ValidatedNel[PackageError, Package.Inferred]] {
        // TODO, we ignore importMap here, we only check earlier we don't
        // have duplicate imports
        case (Package(nm, imports, exports, (stmt, importMap)), recurse) =>

          def getImport[A, B](packF: Package.Inferred,
            exMap: Map[Identifier, NonEmptyList[ExportedName[A]]],
            i: ImportedName[B]): ValidatedNel[PackageError, ImportedName[NonEmptyList[A]]] =
            exMap.get(i.originalName) match {
              case None =>
                Validated.invalidNel(
                  PackageError.UnknownImportName(
                    nm, packF, i,
                    exMap.iterator.flatMap(_._2.toList).toList))
              case Some(exps) =>
                val bs = exps.map(_.tag)
                Validated.valid(i.map(_ => bs))
            }

          def getImportIface[A, B](packF: Package.Interface,
            exMap: Map[Identifier, NonEmptyList[ExportedName[A]]],
            i: ImportedName[B]): ValidatedNel[PackageError, ImportedName[NonEmptyList[A]]] =
            exMap.get(i.originalName) match {
              case None =>
                Validated.invalidNel(
                  PackageError.UnknownImportFromInterface(
                    nm, packF, i,
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
          type ImpRes = Import[Package.Interface, NonEmptyList[Referant[Variance]]]
          def stepImport(i: Import[Package.Resolved, Unit]): FutVal[ImpRes] = {
            val Import(fixpack, items) = i
            Package.unfix(fixpack) match {
              case Right(p) =>
                /*
                 * Here we have a source we need to fully resolve
                 */
                recurse(p).map(_.andThen { packF =>
                  val packInterface = Package.interfaceOf(packF)
                  val exMap = ExportedName.buildExportMap(packF.exports)
                  items.traverse(getImport(packF, exMap, _))
                    .map(Import(packInterface, _))
                })
              case Left(iface) =>
                /*
                 * this import is already an interface, we can stop here
                 */
                val exMap = ExportedName.buildExportMap(iface.exports)
                // this is very fast and does not need to be done in a thread
                Future.successful(items.traverse(getImportIface(iface, exMap, _))
                  .map(Import(iface, _)))
            }
          }

          val inferImports = imports.traverse[FutVal, ImpRes](stepImport(_))(futValid)
          val inferBody =
            inferImports
              .flatMap {
                case Validated.Invalid(err) => Future.successful(Validated.invalid(err))
                case Validated.Valid(imps) =>
                  // run this in a thread
                  Future(Package.inferBody(nm, imps, stmt).map((imps, _)))
              }

          inferBody.map { v =>
            v.andThen { case (imps, program@Program(types, lets, _, _)) =>
              ExportedName.buildExports(nm, exports, types, lets) match {
                case Validated.Valid(exps) =>
                  Validated.valid(Package(nm, imps, exps, program))
                case Validated.Invalid(badPackages) =>
                  Validated.invalid(badPackages.map { n =>
                    PackageError.UnknownExport(n, nm, lets)
                  })
                }
            }
          }
        }

    val fut = ps.toMap.traverse(infer)(futValid)
    // Since this is a finite dag, we have to complete, Inf is safe
    Await.result(fut, Duration.Inf).map(PackageMap(_))
  }

  type DupMap[A] = Map[PackageName, ((A, Package.Parsed), NonEmptyList[(A, Package.Parsed)])]

  def resolveThenInfer[A](
    ps: List[(A, Package.Parsed)],
    ifs: List[Package.Interface])(implicit cpuEC: ExecutionContext): (DupMap[A], ValidatedNel[PackageError, Inferred]) = {
      val (bad, good) = resolveAll(ps, ifs)
      (bad, good.andThen(inferAll(_)))
    }

  def buildSourceMap[F[_]: Foldable, A](parsedFiles: F[((A, LocationMap), Package.Parsed)]): Map[PackageName, (LocationMap, String)] =
    parsedFiles.foldLeft(Map.empty[PackageName, (LocationMap, String)]) { case (map, ((path, lm), pack)) =>
      map.updated(pack.name, (lm, path.toString))
    }

  /** typecheck a list of packages given a list of interface dependencies
   *
   * @param packs a list of parsed packages, along with a key A to tag the source
   * @param ifs the interfaces we are compiling against. If Bosatsu.Predef is not in this list, the default is added
   * @param liftError how to convert package errors into F
   * @param checkDups how to report duplicate package errors
   */
  def typeCheckParsed[F[_]: Apply, A](
    packs: NonEmptyList[((A, LocationMap), Package.Parsed)],
    ifs: List[Package.Interface],
    predefKey: A,
    liftError: FunctionK[ValidatedNel[PackageError, ?], F])(
    checkDups: DupMap[(A, LocationMap)] => F[Unit]
  )(implicit cpuEC: ExecutionContext): F[PackageMap.Inferred] = {
    // if we have passed in a use supplied predef, don't use the internal one
    val useInternalPredef = !ifs.contains { p: Package.Interface => p.name == PackageName.PredefName }
    // Now we have completed all IO, here we do all the checks we need for correctness
    val parsed =
        if (useInternalPredef) Predef.withPredefA[(A, LocationMap)]((predefKey, LocationMap("")), packs.toList)
        else Predef.withPredefImportsA[(A, LocationMap)](packs.toList)

    val (dups, resPacks) = PackageMap.resolveThenInfer(parsed, ifs)

    checkDups(dups) *> liftError(resPacks)
  }
}
