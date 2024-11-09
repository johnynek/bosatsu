package org.bykn.bosatsu

import org.bykn.bosatsu.graph.Memoize
import cats.{Foldable, Monad, Show}
import cats.data.{
  Ior,
  IorT,
  NonEmptyList,
  NonEmptyMap,
  Validated,
  ValidatedNel,
  ReaderT
}
import scala.collection.immutable.SortedMap

import Identifier.Constructor
import IorMethods.IorExtension

import rankn.{DataRepr, TypeEnv}

import cats.implicits._

case class PackageMap[A, B, C, +D](
    toMap: SortedMap[PackageName, Package[A, B, C, D]]
) {
  def +[D1 >: D](pack: Package[A, B, C, D1]): PackageMap[A, B, C, D1] =
    PackageMap(toMap + (pack.name -> pack))

  def ++[D1 >: D](
      packs: Iterable[Package[A, B, C, D1]]
  ): PackageMap[A, B, C, D1] =
    packs.foldLeft(this: PackageMap[A, B, C, D1])(_ + _)

  def getDataRepr(implicit
      ev: Package[A, B, C, D] <:< Package.Typed[Any]
  ): (PackageName, Constructor) => Option[DataRepr] = { (pname, cons) =>
    toMap
      .get(pname)
      .flatMap { pack =>
        ev(pack).types
          .getConstructor(pname, cons)
          .map(_._1.dataRepr(cons))
      }
  }

  def allExternals(implicit
      ev: Package[A, B, C, D] <:< Package.Typed[Any]
  ): Map[PackageName, List[Identifier.Bindable]] =
    toMap.iterator.map { case (name, pack) =>
      (name, ev(pack).externalDefs)
    }.toMap
}

object PackageMap {
  def empty[A, B, C, D]: PackageMap[A, B, C, D] =
    PackageMap(SortedMap.empty)

  def fromIterable[A, B, C, D](
      ps: Iterable[Package[A, B, C, D]]
  ): PackageMap[A, B, C, D] =
    empty[A, B, C, D] ++ ps

  import Package.FixPackage

  type MapF3[A, B, C] = PackageMap[FixPackage[A, B, C], A, B, C]
  type MapF2[A, B] = MapF3[A, A, B]
  type ParsedImp = PackageMap[
    PackageName,
    Unit,
    Unit,
    (List[Statement], ImportMap[PackageName, Unit])
  ]
  type Resolved = MapF2[Unit, (List[Statement], ImportMap[PackageName, Unit])]
  type Typed[+T] = PackageMap[
    Package.Interface,
    NonEmptyList[Referant[Kind.Arg]],
    Referant[Kind.Arg],
    (
        Program[
          TypeEnv[Kind.Arg],
          TypedExpr[T],
          Any
        ],
        ImportMap[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
    )
  ]

  type SourceMap = Map[PackageName, (LocationMap, String)]

  // convenience for type inference
  def toAnyTyped[A](p: Typed[A]): Typed[Any] = p

  type Inferred = Typed[Declaration]

  /** This builds a DAG of actual packages where names have been replaced by the
    * fully resolved packages
    */
  def resolvePackages[A, B, C](
      map: PackageMap[PackageName, A, B, C],
      ifs: List[Package.Interface]
  ): ValidatedNel[PackageError, MapF3[A, B, C]] = {
    val interfaceMap = ifs.iterator.map(iface => (iface.name, iface)).toMap

    def getPackage(
        i: Import[PackageName, A],
        from: Package[PackageName, A, B, C]
    ): ValidatedNel[PackageError, Import[
      Either[Package.Interface, Package[PackageName, A, B, C]],
      A
    ]] =
      map.toMap.get(i.pack) match {
        case Some(pack) => Validated.valid(Import(Right(pack), i.items))
        case None =>
          interfaceMap.get(i.pack) match {
            case Some(iface) =>
              Validated.valid(Import(Left(iface), i.items))
            case None =>
              Validated.invalidNel(
                PackageError.UnknownImportPackage(i.pack, from.name)
              )
          }
      }

    type PackageFix = Package[FixPackage[A, B, C], A, B, C]
    // We use the ReaderT to build the list of imports we are on
    // to detect circular dependencies, if the current package imports itself transitively we
    // want to report the full path
    val step: Package[PackageName, A, B, C] => ReaderT[Either[NonEmptyList[
      PackageError
    ], *], List[PackageName], PackageFix] =
      Memoize.memoizeDagHashed[Package[PackageName, A, B, C], ReaderT[
        Either[NonEmptyList[PackageError], *],
        List[PackageName],
        PackageFix
      ]] { (p, rec) =>
        val edeps = ReaderT
          .ask[Either[NonEmptyList[PackageError], *], List[PackageName]]
          .flatMapF {
            case nonE @ (h :: tail) if nonE.contains(p.name) =>
              Left(
                NonEmptyList.of(
                  PackageError.CircularDependency(p.name, NonEmptyList(h, tail))
                )
              )
            case _ =>
              val deps = p.imports.traverse(
                getPackage(_, p)
              ) // the packages p depends on
              deps.toEither
          }

        edeps
          .flatMap {
            (deps: List[Import[
              Either[Package.Interface, Package[PackageName, A, B, C]],
              A
            ]]) =>
              deps
                .parTraverse { i =>
                  i.pack match {
                    case Right(pack) =>
                      rec(pack)
                        .local[List[PackageName]](
                          p.name :: _
                        ) // add this package into the path of all the deps
                        .map { p =>
                          Import(Package.fix[A, B, C](Right(p)), i.items)
                        }
                    case Left(iface) =>
                      ReaderT.pure[Either[NonEmptyList[PackageError], *], List[
                        PackageName
                      ], Import[FixPackage[A, B, C], A]](
                        Import(Package.fix[A, B, C](Left(iface)), i.items)
                      )
                  }
                }
                .map { imports =>
                  Package(p.name, imports, p.exports, p.program)
                }
          }
      }

    type M = SortedMap[PackageName, PackageFix]
    val r
        : ReaderT[Either[NonEmptyList[PackageError], *], List[PackageName], M] =
      map.toMap.parTraverse(step)

    // we start with no imports on
    val m: Either[NonEmptyList[PackageError], M] = r.run(Nil)

    m.map(PackageMap(_)).toValidated
  }

  /** Convenience method to create a PackageMap then resolve it
    */
  def resolveAll[A: Show](
      ps: List[(A, Package.Parsed)],
      ifs: List[Package.Interface]
  ): Ior[NonEmptyList[PackageError], Resolved] = {

    type AP = (A, Package.Parsed)
    val (nonUnique, unique): (
        SortedMap[PackageName, (AP, NonEmptyList[AP])],
        SortedMap[PackageName, AP]
    ) =
      NonEmptyList.fromList(ps) match {
        case Some(neps) =>
          CollectionUtils
            .uniqueByKey(neps)(_._2.name)
            .fold(
              a => (a.toSortedMap, SortedMap.empty[PackageName, AP]),
              b =>
                (
                  SortedMap.empty[PackageName, (AP, NonEmptyList[AP])],
                  b.toSortedMap
                ),
              (a, b) => (a.toSortedMap, b.toSortedMap)
            )
        case None =>
          (
            SortedMap.empty[PackageName, (AP, NonEmptyList[AP])],
            SortedMap.empty[PackageName, AP]
          )
      }

    def toProg(p: Package.Parsed): (
        Option[PackageError],
        Package[
          PackageName,
          Unit,
          Unit,
          (List[Statement], ImportMap[PackageName, Unit])
        ]
    ) = {

      val (errs0, imap) = ImportMap.fromImports(p.imports) {
        case ((p1, i1), (p2, i2)) =>
          val leftPredef = p1 === PackageName.PredefName
          val rightPredef = p2 === PackageName.PredefName

          if (leftPredef) {
            if (rightPredef) {
              // Both are predef, if one is renamed, choose that, else error
              val r1 = i1.isRenamed
              val r2 = i2.isRenamed
              if (r1 && !r2) ImportMap.Unify.Left
              else if (!r1 && r2) ImportMap.Unify.Right
              else if ((i1 == i2) && !r1) {
                // explicitly importing from predef is allowed.
                // choose one, doesn't matter which they are the same
                ImportMap.Unify.Left
              } else {
                // Both are renamed... this isn't allowed
                ImportMap.Unify.Error
              }
            } else {
              // Predef is replaced by non-predef
              ImportMap.Unify.Right
            }
          } else if (rightPredef) {
            // Predef is replaced by non-predef
            ImportMap.Unify.Left
          } else {
            // neither are Predef, so we error
            ImportMap.Unify.Error
          }
      }
      val errs =
        NonEmptyList
          .fromList(errs0)
          .map(PackageError.DuplicatedImport(p.name, _))

      (errs, p.mapProgram((_, imap)))
    }

    // we know all the package names are unique here
    def foldMap(
        m: Map[PackageName, (A, Package.Parsed)]
    ): (List[PackageError], PackageMap.ParsedImp) = {
      val initPm = PackageMap
        .empty[
          PackageName,
          Unit,
          Unit,
          (List[Statement], ImportMap[PackageName, Unit])
        ]

      m.iterator.foldLeft((List.empty[PackageError], initPm)) {
        case ((errs, pm), (_, (_, pack))) =>
          val (lerrs, pp) = toProg(pack)
          (lerrs.toList ::: errs, pm + pp)
      }
    }

    val (errs, pmap) = foldMap(unique)
    val res = resolvePackages(pmap, ifs)
    // combine the import errors now:
    val check: Ior[NonEmptyList[PackageError], Unit] =
      errs match {
        case Nil =>
          Ior.right(())
        case h :: tail =>
          Ior.left(NonEmptyList(h, tail))
      }
    // keep all the errors
    val nuEr: Ior[NonEmptyList[PackageError], Unit] =
      NonEmptyMap.fromMap(nonUnique) match {
        case Some(nenu) =>
          val paths = nenu.map { case ((a, _), rest) =>
            (a.show, rest.map(_._1.show))
          }
          Ior.left(
            NonEmptyList.one[PackageError](
              PackageError.DuplicatedPackageError(paths)
            )
          )
        case None =>
          Ior.right(())
      }

    (nuEr, check, res.toIor).parMapN((_, _, r) => r)
  }

  /** Infer all the types in a resolved PackageMap
    */
  def inferAll(
      ps: Resolved
  )(implicit cpuEC: Par.EC): Ior[NonEmptyList[PackageError], Inferred] = {

    import Par.F

    // This is unfixed resolved
    type ResolvedU = Package[
      FixPackage[Unit, Unit, (List[Statement], ImportMap[PackageName, Unit])],
      Unit,
      Unit,
      (List[Statement], ImportMap[PackageName, Unit])
    ]

    type FutVal[A] = IorT[F, NonEmptyList[PackageError], A]
    /*
     * We memoize this function to avoid recomputing diamond dependencies
     */
    val infer0: ResolvedU => Par.F[
      Ior[NonEmptyList[PackageError], (TypeEnv[Kind.Arg], Package.Inferred)]
    ] =
      Memoize.memoizeDagFuture[ResolvedU, Ior[NonEmptyList[
        PackageError
      ], (TypeEnv[Kind.Arg], Package.Inferred)]] {
        case (Package(nm, imports, exports, (stmt, imps)), recurse) =>
          val nameToRes = imports.iterator.map { i =>
            val resolved: Package.Resolved = i.pack
            val name = FixType.unfix(resolved) match {
              case Left(iface) => iface.name
              case Right(pack) => pack.name
            }

            (name, resolved)
          }.toMap

          def resolvedImports: ImportMap[Package.Resolved, Unit] =
            imps.traverse[cats.Id, Package.Resolved, Unit] { (p, i) =>
              // the Map.apply below should be safe because the imps
              // are aligned with imports
              (nameToRes(p), i)
            }

          def getImport[A, B](
              packF: Package.Inferred,
              exMap: Map[Identifier, NonEmptyList[ExportedName[A]]],
              i: ImportedName[B]
          ): Ior[NonEmptyList[PackageError], ImportedName[NonEmptyList[A]]] =
            exMap.get(i.originalName) match {
              case None =>
                Ior.left(
                  NonEmptyList.one(
                    PackageError.UnknownImportName(
                      nm,
                      packF.name,
                      packF.program._1.lets.iterator.map { case (n, _, _) =>
                        (n: Identifier, ())
                      }.toMap,
                      i,
                      exMap.iterator.flatMap(_._2.toList).toList
                    )
                  )
                )
              case Some(exps) =>
                val bs = exps.map(_.tag)
                Ior.right(i.map(_ => bs))
            }

          def getImportIface[A, B](
              packF: Package.Interface,
              exMap: Map[Identifier, NonEmptyList[ExportedName[A]]],
              i: ImportedName[B]
          ): Ior[NonEmptyList[PackageError], ImportedName[NonEmptyList[A]]] =
            exMap.get(i.originalName) match {
              case None =>
                Ior.left(
                  NonEmptyList.one(
                    PackageError.UnknownImportFromInterface(
                      nm,
                      packF.name,
                      packF.exports.map(_.name),
                      i,
                      exMap.iterator.flatMap(_._2.toList).toList
                    )
                  )
                )
              case Some(exps) =>
                val bs = exps.map(_.tag)
                Ior.right(i.map(_ => bs))
            }

          /*
           * This resolves imports from PackageNames into fully typed Packages
           *
           * Note the names are not unique after this step because an imported
           * type can have the same name as a constructor. After this step, each
           * distinct object has its own entry in the list
           */
          type IName = NonEmptyList[Referant[Kind.Arg]]

          def stepImport(
              fixpack: Package.Resolved,
              item: ImportedName[Unit]
          ): FutVal[(Package.Interface, ImportedName[IName])] =
            Package.unfix(fixpack) match {
              case Right(p) =>
                /*
                 * Here we have a source we need to fully resolve
                 */
                IorT(recurse(p))
                  .flatMap { case (_, packF) =>
                    val packInterface = Package.interfaceOf(packF)
                    val exMap = packF.exports.groupByNel(_.name)
                    val ior = getImport(packF, exMap, item)
                      .map((packInterface, _))
                    IorT.fromIor(ior)
                  }
              case Left(iface) =>
                /*
                 * this import is already an interface, we can stop here
                 */
                val exMap = iface.exports.groupByNel(_.name)
                // this is very fast and does not need to be done in a thread
                val ior =
                  getImportIface(iface, exMap, item)
                    .map((iface, _))
                IorT.fromIor(ior)
            }

          val inferImports: FutVal[
            ImportMap[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
          ] =
            resolvedImports.parTraverse(stepImport(_, _))

          val inferBody =
            inferImports
              .flatMap { impMap =>
                // run this in a thread
                val ilist = impMap.toList(Package.orderByName)
                IorT(
                  Par.start(
                    Package
                      .inferBodyUnopt(nm, ilist, stmt)
                      .map((ilist, impMap, _))
                  )
                )
              }

          inferBody.flatMap {
            case (ilist, imap, (fte, program @ Program(types, lets, _, _))) =>
              val ior = ExportedName
                .buildExports(nm, exports, types, lets) match {
                case Validated.Valid(exports) =>
                  // We have a result, which we can continue to check
                  val pack = Package(nm, ilist, exports, (program, imap))
                  // We have to check the "customs" before any normalization
                  // or optimization
                  PackageCustoms(pack) match {
                    case Validated.Valid(p1) => Ior.right((fte, p1))
                    case Validated.Invalid(errs) =>
                      val res = (fte, pack)
                      Ior.both(errs.toNonEmptyList, res)
                  }
                case Validated.Invalid(badPackages) =>
                  Ior.left(badPackages.map { n =>
                    PackageError.UnknownExport(n, nm, lets): PackageError
                  })
              }
              IorT.fromIor(ior)
          }.value
      }

    /*
     * Since Par.F is starts computation when start is called
     * we want to start all the computations *then* collect
     * the result together
     */
    val infer: ResolvedU => Par.F[
      Ior[NonEmptyList[PackageError], Package.Inferred]
    ] =
      infer0.andThen { parF =>
        // As soon as each Par.F is complete, we can start normalizing that one
        Monad[Par.F].flatMap(parF) { ior =>
          ior.traverse { case (fte, pack) =>
            Par.start {
              val optPack = pack.copy(program =
                (
                  TypedExprNormalization.normalizeProgram(
                    pack.name,
                    fte,
                    pack.program._1
                  ),
                  pack.program._2
                )
              )
              Package.discardUnused(optPack)
            }
          }
        }
      }

    val fut = ps.toMap.parTraverse(infer.andThen(IorT(_)))

    // Wait until all the resolution is complete
    Par
      .await(fut.value)
      .map(PackageMap(_))
  }

  def resolveThenInfer[A: Show](
      ps: List[(A, Package.Parsed)],
      ifs: List[Package.Interface]
  )(implicit cpuEC: Par.EC): Ior[NonEmptyList[PackageError], Inferred] =
    resolveAll(ps, ifs).flatMap(inferAll)

  def buildSourceMap[F[_]: Foldable, A](
      parsedFiles: F[((A, LocationMap), Package.Parsed)]
  ): Map[PackageName, (LocationMap, String)] =
    parsedFiles.foldLeft(Map.empty[PackageName, (LocationMap, String)]) {
      case (map, ((path, lm), pack)) =>
        map.updated(pack.name, (lm, path.toString))
    }

  /** typecheck a list of packages given a list of interface dependencies
    *
    * @param packs
    *   a list of parsed packages, along with a key A to tag the source
    * @param ifs
    *   the interfaces we are compiling against. If Bosatsu.Predef is not in
    *   this list, the default is added
    */
  def typeCheckParsed[A: Show](
      packs: NonEmptyList[((A, LocationMap), Package.Parsed)],
      ifs: List[Package.Interface],
      predefKey: A
  )(implicit
      cpuEC: Par.EC
  ): Ior[NonEmptyList[PackageError], PackageMap.Inferred] = {
    // if we have passed in a use supplied predef, don't use the internal one
    val useInternalPredef = !ifs.exists { (p: Package.Interface) =>
      p.name == PackageName.PredefName
    }
    // Now we have completed all IO, here we do all the checks we need for correctness
    val parsed =
      if (useInternalPredef)
        withPredefA[(A, LocationMap)](
          (predefKey, LocationMap("")),
          packs.toList
        )
      else withPredefImportsA[(A, LocationMap)](packs.toList)

    PackageMap
      .resolveThenInfer[A](parsed.map { case ((a, _), p) => (a, p) }, ifs)
  }

  /** Here is the fully compiled Predef
    */
  val predefCompiled: Package.Inferred = {
    import DirectEC.directEC

    // implicit val showUnit: Show[Unit] = Show.show[Unit](_ => "predefCompiled")
    val inferred = PackageMap
      .resolveThenInfer(((), Package.predefPackage) :: Nil, Nil)
      .strictToValidated

    inferred match {
      case Validated.Valid(v) =>
        v.toMap.get(PackageName.PredefName) match {
          case None =>
            sys.error(
              "internal error: predef package not found after compilation"
            )
          case Some(inf) => inf
        }
      case Validated.Invalid(errs) =>
        val map = Map(
          PackageName.PredefName -> (
            LocationMap(
              Predef.predefString
            ),
            "<predef>"
          )
        )
        errs.iterator.foreach { err =>
          println(err.message(map, LocationMap.Colorize.None))
        }
        sys.error("expected no errors")
    }
  }

  private val predefImportList = predefCompiled.exports
    .map(_.name)
    .distinct
    .sorted
    .map(ImportedName.OriginalName(_, ()))

  private val predefImports: Import[PackageName, Unit] =
    Import(PackageName.PredefName, NonEmptyList.fromList(predefImportList).get)

  private def withPredefImportsA[A](
      ps: List[(A, Package.Parsed)]
  ): List[(A, Package.Parsed)] =
    ps.map { case (a, p) => (a, p.withImport(predefImports)) }

  def withPredef(ps: List[Package.Parsed]): List[Package.Parsed] =
    Package.predefPackage :: ps.map(_.withImport(predefImports))

  def withPredefA[A](
      predefA: A,
      ps: List[(A, Package.Parsed)]
  ): List[(A, Package.Parsed)] =
    (predefA, Package.predefPackage) :: withPredefImportsA(ps)

}
