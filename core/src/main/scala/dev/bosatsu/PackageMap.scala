package dev.bosatsu

import dev.bosatsu.graph.{Dag, Memoize, Toposort}
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
import Package.ResolvedMethods

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
  ): Map[PackageName, List[(Identifier.Bindable, rankn.Type)]] =
    toMap.iterator.map { case (name, pack) =>
      val tpack = ev(pack)
      (
        name,
        tpack.externalDefs.map { n =>
          (
            n,
            tpack.types
              .getExternalValue(name, n)
              .getOrElse(
                sys.error(s"invariant violation, unknown type: $name $n")
              )
          )
        }
      )
    }.toMap

  def testValues(implicit
      ev: Package[A, B, C, D] <:< Package.Typed[Any]
  ): Map[PackageName, Identifier.Bindable] =
    toMap.iterator.flatMap { case (n, pack) =>
      Package.testValue(ev(pack)).iterator.map { case (bn, _, _) =>
        (n, bn)
      }
    }.toMap

  def topoSort(implicit
      ev: Package[A, B, C, D] <:< Package.Typed[Any]
  ): Toposort.Result[PackageName] = {

    val packNames = toMap.keys.iterator.toList.sorted

    def nfn(p: PackageName): List[PackageName] =
      toMap.get(p) match {
        case None       => Nil
        case Some(pack) =>
          val tpack = ev(pack)
          tpack.imports.map(_.pack.name).sorted
      }

    Toposort.sort(packNames)(nfn)
  }
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
  type Interface = PackageMap[Nothing, Nothing, Referant[Kind.Arg], Unit]

  type SourceMap = Map[PackageName, (LocationMap, String)]

  // convenience for type inference
  def toAnyTyped[A](p: Typed[A]): Typed[Any] = p

  def filterLets[A](
      p: Typed[A],
      keep: ((PackageName, Identifier)) => Boolean
  ): Typed[A] = {
    val kept = p.toMap.iterator.map { case (_, pack) =>
      val name = pack.name
      pack.filterLets(nm => keep((name, nm)))
    }.toList

    fromIterable(kept)
  }

  def treeShake[A](
      p: Typed[A],
      roots: Set[(PackageName, Identifier)]
  ): Typed[A] = {
    type Ident = (PackageName, Identifier)

    def dependency(a: Ident): Iterable[Ident] = {
      val (pn, ident) = a
      ident match {
        case b: Identifier.Bindable =>
          p.toMap.get(pn) match {
            case Some(pack) =>
              pack.program._1.getLet(b).toList.flatMap(_._2.globals)
            case None => Nil
          }
        case _ => Nil
      }
    }

    val keep = Dag.transitiveSet(roots.toList.sorted)(dependency)
    filterLets(p, keep)
  }

  type Inferred = Typed[Declaration]

  /** This builds a DAG of actual packages where on Import the PackageName have
    * been replaced by the Either a Package.Interface (which gives exports only)
    * or this same recursive structure.
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
        case None       =>
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
    type ErrorOr[A] = Either[NonEmptyList[PackageError], A]
    // We use the ReaderT to build the list of imports we are on
    // to detect circular dependencies, if the current package imports itself transitively we
    // want to report the full path
    val step: Package[PackageName, A, B, C] => ReaderT[ErrorOr, List[
      PackageName
    ], PackageFix] =
      Memoize.memoizeDagHashed[Package[PackageName, A, B, C], ReaderT[
        ErrorOr,
        List[PackageName],
        PackageFix
      ]] { (p, rec) =>
        val edeps = ReaderT
          .ask[ErrorOr, List[PackageName]]
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
                      ReaderT.pure[ErrorOr, List[PackageName], Import[
                        FixPackage[A, B, C],
                        A
                      ]](
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
    val r: ReaderT[ErrorOr, List[PackageName], M] =
      map.toMap.parTraverse(step)

    // we start with no imports on
    val m: ErrorOr[M] = r.run(Nil)

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
        m: SortedMap[PackageName, (A, Package.Parsed)]
    ): (List[PackageError], PackageMap.ParsedImp) = {
      val initPm = PackageMap
        .empty[
          PackageName,
          Unit,
          Unit,
          (List[Statement], ImportMap[PackageName, Unit])
        ]

      // since the map is sorted, this order is deteriministic
      m.foldLeft((List.empty[PackageError], initPm)) {
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
        case Nil       => Ior.right(())
        case h :: tail =>
          Ior.left(NonEmptyList(h, tail))
      }
    // keep all the errors
    val nuEr: Ior[NonEmptyList[PackageError], Unit] =
      NonEmptyMap.fromMap(nonUnique) match {
        case None       => Ior.right(())
        case Some(nenu) =>
          val paths = nenu.map { case ((a, _), rest) =>
            (a.show, rest.map(_._1.show))
          }
          Ior.left(
            NonEmptyList.one[PackageError](
              PackageError.DuplicatedPackageError(paths)
            )
          )
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
            (resolved.name, resolved)
          }.toMap

          def resolvedImports: ImportMap[Package.Resolved, Unit] =
            imps.traverse[cats.Id, Package.Resolved, Unit] { (p, i) =>
              // the Map.apply below should be safe because the imps
              // are aligned with imports
              (nameToRes(p), i)
            }

          /*
           * This resolves imports from PackageNames into fully typed Packages
           *
           * Note the names are not unique after this step because an imported
           * type can have the same name as a constructor. After this step, each
           * distinct object has its own entry in the list
           */

          val inferImports: FutVal[
            ImportMap[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
          ] = {
            // here we just need the interface, not the TypeEnv
            val rec1 = recurse.andThen(res => IorT(res).map(_._2))

            resolvedImports.parTraverse {
              (fixpack: Package.Resolved, item: ImportedName[Unit]) =>
                fixpack
                  .importName(nm, item)(rec1(_))
                  .flatMap { either =>
                    IorT.fromEither(either.left.map(NonEmptyList.one(_)))
                  }
            }
          }

          val inferBody =
            inferImports.flatMap { impMap =>
              // run this in a thread
              val ilist = impMap.toList(using Package.orderByName)
              IorT(
                Par.start(
                  Package
                    .inferBodyUnopt(nm, ilist, stmt)
                    .map((ilist, impMap, _))
                )
              )
            }

          inferBody.flatMap { case (ilist, imap, (fte, program)) =>
            val ior =
              PackageCustoms
                .assemble(nm, ilist, imap, exports, program)
                .map((fte, _))
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
  val predefCompiled: Package.Inferred = Par.noParallelism {

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
    Package.predefPackage :: Package.numericPackage :: Package.ioPackage :: ps.map(_.withImport(predefImports))

  def withPredefA[A](
      predefA: A,
      ps: List[(A, Package.Parsed)]
  ): List[(A, Package.Parsed)] =
    (predefA, Package.predefPackage) :: (predefA, Package.numericPackage) :: (predefA, Package.ioPackage) :: withPredefImportsA(ps)

}
