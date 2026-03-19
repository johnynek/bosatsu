package dev.bosatsu

import dev.bosatsu.graph.{CanPromise, Dag, Memoize, Toposort}
import cats.{Applicative, Foldable, Monad, Parallel, Show}
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
import dev.bosatsu.cache.{CompileCache, InferCache, InferPhases}
import dev.bosatsu.hashing.{Algo, HashValue}

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
    testEntries.collect {
      case (pn, Right(Package.TestEntry.PlainTest(bindable, _, _))) =>
        (pn, bindable)
    }

  def testEntries(implicit
      ev: Package[A, B, C, D] <:< Package.Typed[Any]
  ): Map[PackageName, Either[Package.TestDiscoveryError, Package.TestEntry[
    Any
  ]]] =
    toMap.iterator.flatMap { case (n, pack) =>
      Package.testEntry(ev(pack)) match {
        case Right(Some(entry)) => Iterator.single((n, Right(entry)))
        case Right(None)        => Iterator.empty
        case Left(err)          => Iterator.single((n, Left(err)))
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
  private def circularDependencyCycle(
      err: PackageError.CircularDependency[?, ?, ?]
  ): List[PackageName] = {
    val path = err.path.toList
    val idx = path.indexOf(err.from)
    val nonRepeating =
      if (idx < 0) path.reverse
      else path.take(idx).reverse

    err.from :: nonRepeating ::: (err.from :: Nil)
  }

  private def canonicalCycle(cycle: List[PackageName]): List[PackageName] =
    cycle match {
      case first :: rest =>
        val nodes = first :: rest.dropRight(1)
        val min = nodes.min
        val idx = nodes.indexOf(min)
        val rotated = nodes.drop(idx) ::: nodes.take(idx)
        rotated ::: (rotated.head :: Nil)
      case Nil => Nil
    }

  private def normalizeCircularDependencyErrors(
      errs: NonEmptyList[PackageError]
  ): NonEmptyList[PackageError] = {
    val circularAndOther =
      errs.toList.foldLeft(
        (List.empty[List[PackageName]], List.empty[PackageError])
      ) { case ((cycles, others), err) =>
        err match {
          case circular: PackageError.CircularDependency[?, ?, ?] =>
            (circularDependencyCycle(circular) :: cycles, others)
          case other =>
            (cycles, other :: others)
        }
      }

    val (rawCyclesRev, otherErrsRev) = circularAndOther

    if (rawCyclesRev.isEmpty) errs
    else {
      val canonicalCycles = rawCyclesRev.reverse.map(canonicalCycle)
      val byStart = canonicalCycles.groupBy(_.head)

      val minimalCycles: List[List[PackageName]] = byStart.iterator
        .flatMap { case (_, cycles) =>
          val minSize = cycles.iterator.map(_.size).min
          cycles.filter(_.size == minSize)
        }
        .toList

      implicit val cycleOrdering: Ordering[List[PackageName]] =
        ListOrdering.onType(PackageName.packageNameOrdering)

      val normalizedCircularErrs: List[PackageError] =
        minimalCycles
          .distinct
          .sorted
          .map { cycle =>
            val from = cycle.head
            val path = NonEmptyList.fromListUnsafe(cycle.tail)
            PackageError.CircularDependency(from, path)
          }

      NonEmptyList.fromListUnsafe(normalizedCircularErrs ::: otherErrsRev.reverse)
    }
  }

  def empty[A, B, C, D]: PackageMap[A, B, C, D] =
    PackageMap(SortedMap.empty)

  def fromIterable[A, B, C, D](
      ps: Iterable[Package[A, B, C, D]]
  ): PackageMap[A, B, C, D] =
    empty[A, B, C, D] ++ ps

  import Package.FixPackage

  type MapF3[A, B, C] = PackageMap[FixPackage[A, B, C], A, B, C]
  type MapF2[A, B] = MapF3[A, A, B]
  final case class SourceUnit[F[_], A](
      sourceKey: A,
      locationMap: LocationMap,
      packageName: PackageName,
      imports: List[Import[PackageName, Unit]],
      exports: List[ExportedName[Unit]],
      sourceHash: HashValue[Algo.Blake3],
      loadParsed: F[Package.Parsed]
  ) {
    def withImport(i: Import[PackageName, Unit]): SourceUnit[F, A] =
      copy(imports = i :: imports)
  }
  object SourceUnit {
    def fromParsed[F[_]: Applicative, A](
        sourceKey: A,
        locationMap: LocationMap,
        parsed: Package.Parsed
    ): SourceUnit[F, A] =
      SourceUnit(
        sourceKey = sourceKey,
        locationMap = locationMap,
        packageName = parsed.name,
        imports = parsed.imports,
        exports = parsed.exports,
        sourceHash = CompileCache.sourceExprHash(parsed),
        loadParsed = Applicative[F].pure(parsed)
      )

    def fromParsed[F[_]: Applicative, A](
        parsed: ((A, LocationMap), Package.Parsed)
    ): SourceUnit[F, A] =
      fromParsed(parsed._1._1, parsed._1._2, parsed._2)

    def fromParsedWithoutLocation[F[_]: Applicative, A](
        parsed: (A, Package.Parsed)
    ): SourceUnit[F, A] =
      fromParsed(parsed._1, LocationMap(""), parsed._2)

    def predef[F[_]: Applicative, A](
        predefKey: A,
        mode: CompileOptions.Mode
    ): SourceUnit[F, A] =
      fromParsed(predefKey, LocationMap(""), Package.predefPackageForMode(mode))
  }
  type SourceImp[F[_], A] = PackageMap[
    PackageName,
    Unit,
    Unit,
    (SourceUnit[F, A], ImportMap[PackageName, Unit])
  ]
  type ResolvedSource[F[_], A] = MapF2[
    Unit,
    (SourceUnit[F, A], ImportMap[PackageName, Unit])
  ]
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

  extension [A](opt: Option[A])
    inline def expect(inline message: => String): A =
      if (opt.isDefined) opt.get
      else sys.error(message)

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

    m.leftMap(normalizeCircularDependencyErrors).map(PackageMap(_)).toValidated
  }

  private def resolveAllSourceUnits[F[_], A: Show](
      ps: List[SourceUnit[F, A]],
      ifs: List[Package.Interface]
  ): Ior[NonEmptyList[PackageError], ResolvedSource[F, A]] = {
    type SU = SourceUnit[F, A]
    val (nonUnique, unique): (
        SortedMap[PackageName, (SU, NonEmptyList[SU])],
        SortedMap[PackageName, SU]
    ) =
      NonEmptyList.fromList(ps) match {
        case Some(neps) =>
          CollectionUtils
            .uniqueByKey(neps)(_.packageName)
            .fold(
              a => (a.toSortedMap, SortedMap.empty[PackageName, SU]),
              b => (SortedMap.empty[PackageName, (SU, NonEmptyList[SU])], b.toSortedMap),
              (a, b) => (a.toSortedMap, b.toSortedMap)
            )
        case None =>
          (
            SortedMap.empty[PackageName, (SU, NonEmptyList[SU])],
            SortedMap.empty[PackageName, SU]
          )
      }

    def toProg(
        source: SU
    ): (
        Option[PackageError],
        Package[
          PackageName,
          Unit,
          Unit,
          (SU, ImportMap[PackageName, Unit])
        ]
    ) = {
      val (errs0, imap) = ImportMap.fromImports(source.imports) {
        case ((p1, i1), (p2, i2)) =>
          val leftPredef = p1 == PackageName.PredefName
          val rightPredef = p2 == PackageName.PredefName

          if (leftPredef) {
            if (rightPredef) {
              val r1 = i1.isRenamed
              val r2 = i2.isRenamed
              if (r1 && !r2) ImportMap.Unify.Left
              else if (!r1 && r2) ImportMap.Unify.Right
              else if ((i1 === i2) && !r1) ImportMap.Unify.Left
              else ImportMap.Unify.Error
            } else ImportMap.Unify.Right
          } else if (rightPredef) ImportMap.Unify.Left
          else ImportMap.Unify.Error
      }
      val errs =
        NonEmptyList
          .fromList(errs0)
          .map(PackageError.DuplicatedImport(source.packageName, _))

      (
        errs,
        Package(source.packageName, source.imports, source.exports, (source, imap))
      )
    }

    def foldMap(
        m: SortedMap[PackageName, SU]
    ): (List[PackageError], SourceImp[F, A]) = {
      val initPm = PackageMap.empty[
        PackageName,
        Unit,
        Unit,
        (SU, ImportMap[PackageName, Unit])
      ]

      m.foldLeft((List.empty[PackageError], initPm)) { case ((errs, pm), (_, source)) =>
        val (lerrs, pp) = toProg(source)
        (lerrs.toList ::: errs, pm + pp)
      }
    }

    val (errs, pmap) = foldMap(unique)
    val res = resolvePackages(pmap, ifs)
    val check: Ior[NonEmptyList[PackageError], Unit] =
      errs match {
        case Nil       => Ior.right(())
        case h :: tail => Ior.left(NonEmptyList(h, tail))
      }
    val nuEr: Ior[NonEmptyList[PackageError], Unit] =
      NonEmptyMap.fromMap(nonUnique) match {
        case None       => Ior.right(())
        case Some(nenu) =>
          val paths = nenu.map { case (source, rest) =>
            (source.sourceKey.show, rest.map(_.sourceKey.show))
          }
          Ior.left(
            NonEmptyList.one[PackageError](
              PackageError.DuplicatedPackageError(paths)
            )
          )
      }

    (nuEr, check, res.toIor).parMapN((_, _, r) => r)
  }

  private def inferAllSourceUnits[F[_]: Monad: Parallel: CanPromise, A](
      ps: ResolvedSource[F, A],
      compileOptions: CompileOptions,
      cache: InferCache[F],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], Inferred]] = {
    type SourceProgram = (SourceUnit[F, A], ImportMap[PackageName, Unit])
    type ResolvedFix = FixPackage[Unit, Unit, SourceProgram]
    type ResolvedU = Package[
      ResolvedFix,
      Unit,
      Unit,
      SourceProgram
    ]
    type ErrorOr[A1] = Ior[NonEmptyList[PackageError], A1]
    type CacheDepHash = cache.DepHash

    final case class InferredPack[H](
        inferred: Package.Inferred,
        depInterface: Package.Interface,
        depInterfaceHash: H
    )

    val resolvedByName: SortedMap[PackageName, ResolvedU] = ps.toMap

    def sourceDeps(pack: ResolvedU): List[PackageName] =
      pack.imports.iterator
        .flatMap(imp => Package.unfix(imp.pack).toOption.map(_.name))
        .toList
        .distinct

    val isDag = Toposort
      .sort(resolvedByName.keys)(nm => sourceDeps(resolvedByName(nm)))
      .toSuccess
      .isDefined

    if (!isDag) {
      sys.error("invariant violation: resolved package graph has a cycle")
    }

    def toInferredPack(inferred: Package.Inferred): F[InferredPack[CacheDepHash]] = {
      val depInterface = phases.dependencyInterface(inferred)
      cache.dependencyHash(depInterface).map { depInterfaceHash =>
        InferredPack(inferred, depInterface, depInterfaceHash)
      }
    }

    val inferPack: ResolvedU => F[ErrorOr[InferredPack[CacheDepHash]]] =
      Memoize.memoizeDag[F, ResolvedU, ErrorOr[InferredPack[CacheDepHash]]] {
        case (pack, recurse) =>
          pack match {
            case Package(nm, imports, exports, (source, imps)) =>
              val depResultsF
                  : F[ErrorOr[SortedMap[PackageName, InferredPack[CacheDepHash]]]] =
                imports.foldLeft(
                  Monad[F].pure(
                    Ior.right[NonEmptyList[PackageError], SortedMap[
                      PackageName,
                      InferredPack[CacheDepHash]
                    ]](SortedMap.empty)
                  )
                ) { (accF, imp) =>
                  Package.unfix(imp.pack) match {
                    case Left(_)        => accF
                    case Right(depPack) =>
                      val nextF: F[ErrorOr[(PackageName, InferredPack[CacheDepHash])]] =
                        recurse(depPack).map(_.map(dep => dep.inferred.name -> dep))
                      (accF, nextF).parMapN { (acc, next) =>
                        (acc, next).parMapN { (deps, dep) =>
                          deps.updated(dep._1, dep._2)
                        }
                      }
                  }
                }

              IorT(depResultsF)
                .flatMap { depResults =>
                  def depPackResult(depPack: ResolvedU): ErrorOr[Package.Inferred] =
                    Ior.right(depResults.get(depPack.name).expect {
                      s"invariant violation: missing dependency result for ${depPack.name}"
                    }.inferred)

                  def depPackMeta(
                      depPack: ResolvedU
                  ): ErrorOr[InferredPack[CacheDepHash]] =
                    Ior.right(depResults.get(depPack.name).expect {
                      s"invariant violation: missing dependency result for ${depPack.name}"
                    })

                  def depInterfaces
                      : ErrorOr[SortedMap[PackageName, Package.Interface]] =
                    imports.foldLeft(
                      Ior.right[NonEmptyList[PackageError], SortedMap[
                        PackageName,
                        Package.Interface
                      ]](SortedMap.empty)
                    ) { (acc, imp) =>
                      val next: ErrorOr[(PackageName, Package.Interface)] =
                        Package.unfix(imp.pack) match {
                          case Right(depPack) =>
                            depPackMeta(depPack).map { inferredDep =>
                              inferredDep.inferred.name -> inferredDep.depInterface
                            }
                          case Left(iface)    =>
                            Ior.right(iface.name -> iface)
                        }

                      (acc, next).parMapN { (ifaces, iface) =>
                        ifaces.updated(iface._1, iface._2)
                      }
                    }

                  val depInterfaceHashesF
                      : F[ErrorOr[SortedMap[PackageName, CacheDepHash]]] =
                    imports.foldLeft(
                      Monad[F].pure(
                        Ior.right[NonEmptyList[PackageError], SortedMap[
                          PackageName,
                          CacheDepHash
                        ]](SortedMap.empty)
                      )
                    ) { (accF, imp) =>
                      val nextF: F[ErrorOr[(PackageName, CacheDepHash)]] =
                        Package.unfix(imp.pack) match {
                          case Right(depPack) =>
                            Monad[F].pure(
                              depPackMeta(depPack).map { inferredDep =>
                                inferredDep.inferred.name -> inferredDep.depInterfaceHash
                              }
                            )
                          case Left(iface)    =>
                            cache
                              .dependencyHash(iface)
                              .map(hash => Ior.right(iface.name -> hash))
                        }

                      (accF, nextF).parMapN { (acc, next) =>
                        (acc, next).parMapN { (hashes, hash) =>
                          hashes.updated(hash._1, hash._2)
                        }
                      }
                    }

                  def inferOnMiss(
                      depIfaces: SortedMap[PackageName, Package.Interface]
                  ): F[ErrorOr[Package.Inferred]] = {
                    def resolvedName(resolved: ResolvedFix): PackageName =
                      Package.unfix(resolved) match {
                        case Left(iface) => iface.name
                        case Right(pack) => pack.name
                      }

                    def importResolvedName[F2[_], T](
                        resolved: ResolvedFix,
                        fromPackage: PackageName,
                        item: ImportedName[Unit]
                    )(recurse: ResolvedU => F2[Package.Typed[T]])(implicit
                        F2: Applicative[F2]
                    ): F2[Either[
                      PackageError,
                      (
                          Package.Interface,
                          ImportedName[
                            NonEmptyList[Referant[Kind.Arg]]
                          ]
                      )
                    ]] =
                      Package.unfix(resolved) match {
                        case Right(p) =>
                          recurse(p).map { packF =>
                            val packInterface = Package.interfaceOf(packF)
                            packF
                              .getImport(fromPackage, item)
                              .map((packInterface, _))
                          }
                        case Left(iface) =>
                          F2.pure(
                            iface
                              .getImportIface(fromPackage, item)
                              .map((iface, _))
                          )
                      }

                    val nameToRes: Map[PackageName, ResolvedFix] = imports.iterator
                      .map { i =>
                        val resolved = i.pack
                        (resolvedName(resolved), resolved)
                      }
                      .toMap

                    val resolvedImports: ImportMap[ResolvedFix, Unit] =
                      imps.traverse[cats.Id, ResolvedFix, Unit] { (p, i) =>
                        (nameToRes(p), i)
                      }

                    val inferImports
                        : ErrorOr[
                          ImportMap[
                            Package.Interface,
                            NonEmptyList[Referant[Kind.Arg]]
                          ]
                        ] = {
                      val rec1: ResolvedU => ErrorOr[Package.Inferred] = depPackResult
                      resolvedImports.parTraverse {
                        (fixpack: ResolvedFix, item: ImportedName[Unit]) =>
                          importResolvedName[[A1] =>> ErrorOr[A1], Declaration](
                            fixpack,
                            nm,
                            item
                          )(rec1)
                            .flatMap { either =>
                              Ior.fromEither(either.left.map(NonEmptyList.one(_)))
                            }
                      }
                    }

                    source.loadParsed.flatMap { parsed =>
                      inferImports.flatTraverse { impMap =>
                        val ilist = impMap.toList(using Package.orderByName)
                        // Type inference plus assembly is the heaviest
                        // per-package CPU step on a cache miss.
                        summon[CanPromise[F]].compute {
                          for {
                            (_, program) <- Package.inferBodyUnopt(
                              nm,
                              ilist,
                              exports,
                              parsed.program
                            )
                            asm <- PackageCustoms.assemble(
                              nm,
                              ilist,
                              impMap,
                              exports,
                              program
                            )
                          } yield phases.finishPackage(asm, depIfaces, compileOptions)
                        }
                      }
                    }
                  }

                  import IorT.{fromIor, liftF}

                  (for {
                    depIfaceHashes <- IorT(depInterfaceHashesF)
                    key <- liftF(
                      cache.generateKey(
                        nm,
                        source.sourceHash,
                        depIfaceHashes,
                        compileOptions,
                        CompileCache.compilerIdentity,
                        phases.id
                      )
                    )
                    getRes <- liftF(cache.get(key))
                    res <- getRes match {
                      case Some(hit) =>
                        IorT.rightT[F, NonEmptyList[PackageError]](hit)
                      case None      =>
                        for {
                          depIfaces <- fromIor[F](depInterfaces)
                          compiled <- IorT(inferOnMiss(depIfaces))
                          _ <- liftF(cache.put(key, compiled))
                        } yield compiled
                    }
                    inferredPack <- liftF(toInferredPack(res))
                  } yield inferredPack)
                }
                .value
          }
      }

    val allResults: F[SortedMap[PackageName, ErrorOr[InferredPack[CacheDepHash]]]] =
      resolvedByName.values.toList
        .parTraverse { pack =>
          inferPack(pack).map(pack.name -> _)
        }
        .map(SortedMap.from(_))

    allResults.map { resultMap =>
      val missingKeys = resolvedByName.keySet.diff(resultMap.keySet).toList.sorted
      if (missingKeys.nonEmpty) {
        sys.error(
          s"invariant violation: missing inference results for: ${missingKeys.mkString(", ")}"
        )
      } else {
        val sequenced =
          resultMap.traverse { result =>
            result match {
              case Ior.Left(errs)     =>
                Ior.both(errs, Option.empty[Package.Inferred])
              case Ior.Right(inferred) =>
                Ior.right(Some(inferred.inferred))
              case Ior.Both(errs, inferred) =>
                Ior.both(errs, Some(inferred.inferred))
            }
          }

        val deduped = sequenced.leftMap { errs =>
          NonEmptyList.fromListUnsafe(errs.toList.distinct)
        }

        deduped match {
          case Ior.Right(entries) =>
            Ior.right(
              PackageMap(
                SortedMap.from(
                  entries.iterator.collect { case (name, Some(pack)) =>
                    (name, pack)
                  }
                )
              )
            )
          case Ior.Both(errs, entries) =>
            val inferred =
              PackageMap(
                SortedMap.from(
                  entries.iterator.collect { case (name, Some(pack)) =>
                    (name, pack)
                  }
                )
              )
            if (inferred.toMap.isEmpty) Ior.left(errs)
            else Ior.both(errs, inferred)
          case Ior.Left(errs) =>
            Ior.left(errs)
        }
      }
    }
  }

  private def resolveThenInferSourceUnits[F[_]: Monad: Parallel: CanPromise, A: Show](
      ps: List[SourceUnit[F, A]],
      ifs: List[Package.Interface],
      compileOptions: CompileOptions,
      cache: InferCache[F],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], Inferred]] =
    IorT(
      // Resolving imports and validating the full source graph is pure CPU
      // work over the whole input set, so keep it on the compute pool.
      summon[CanPromise[F]].compute {
        resolveAllSourceUnits(ps, ifs)
      }
    )
      .flatMap(resolved =>
        IorT(inferAllSourceUnits(resolved, compileOptions, cache, phases))
      )
      .value

  private def buildSourceMapImpl[F[_]: Foldable, A, B: Show](
      values: F[A]
  )(
      packageNameOf: A => PackageName,
      locationMapOf: A => LocationMap,
      sourceKeyOf: A => B
  ): Map[PackageName, (LocationMap, String)] =
    values.foldLeft(Map.empty[PackageName, (LocationMap, String)]) {
      case (map, value) =>
        map.updated(
          packageNameOf(value),
          (locationMapOf(value), sourceKeyOf(value).show)
        )
    }

  def buildSourceMap[F[_]: Foldable, A: Show](
      parsedFiles: F[((A, LocationMap), Package.Parsed)]
  ): Map[PackageName, (LocationMap, String)] =
    buildSourceMapImpl(parsedFiles)(_._2.name, _._1._2, _._1._1)

  def buildSourceMapFromSources[G[_]: Foldable, F[_], A: Show](
      sources: G[SourceUnit[F, A]]
  ): Map[PackageName, (LocationMap, String)] =
    buildSourceMapImpl(sources)(_.packageName, _.locationMap, _.sourceKey)

  private def withPredefImportsSourceUnits[F[_], A](
      sources: List[SourceUnit[F, A]],
      predefImports: Import[PackageName, Unit]
  ): List[SourceUnit[F, A]] =
    sources.map(_.withImport(predefImports))

  private def withEffectivePredefSources[F[_]: Monad, A](
      sources: NonEmptyList[SourceUnit[F, A]],
      ifs: List[Package.Interface],
      predefKey: A,
      mode: CompileOptions.Mode
  ): List[SourceUnit[F, A]] = {
    val predefIface = ifs.find(_.name == PackageName.PredefName)
    val withPredefImports =
      withPredefImportsSourceUnits(
        sources.toList,
        predefIface.fold(predefImportsForMode(mode))(iface =>
          predefImportsFromExports(iface.exports)
        )
      )

    predefIface match {
      case None       => SourceUnit.predef(predefKey, mode) :: withPredefImports
      case Some(_)    => withPredefImports
    }
  }

  def typeCheckSources[F[_]: Monad: Parallel: CanPromise, A: Show](
      sources: NonEmptyList[SourceUnit[F, A]],
      ifs: List[Package.Interface],
      predefKey: A,
      compileOptions: CompileOptions,
      cache: InferCache[F],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], PackageMap.Inferred]] =
    PackageMap.resolveThenInferSourceUnits[F, A](
      withEffectivePredefSources(
        sources,
        ifs,
        predefKey,
        compileOptions.mode
      ),
      ifs,
      compileOptions,
      cache,
      phases
    )

  def typeCheckParsed[F[_]: Monad: Parallel: CanPromise, A: Show](
      packs: NonEmptyList[((A, LocationMap), Package.Parsed)],
      ifs: List[Package.Interface],
      predefKey: A,
      compileOptions: CompileOptions,
      cache: InferCache[F],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], PackageMap.Inferred]] =
    typeCheckSources(
      NonEmptyList.fromListUnsafe(
        packs.toList.map(SourceUnit.fromParsed[F, A])
      ),
      ifs,
      predefKey,
      compileOptions,
      cache,
      phases
    )

  def resolveThenInfer[F[_]: Monad: Parallel: CanPromise, A: Show](
      ps: List[(A, Package.Parsed)],
      ifs: List[Package.Interface],
      compileOptions: CompileOptions,
      cache: InferCache[F],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], Inferred]] =
    resolveThenInferSourceUnits(
      ps.map(SourceUnit.fromParsedWithoutLocation[F, A]),
      ifs,
      compileOptions,
      cache,
      phases
    )

  def resolveThenInfer[A: Show](
      ps: List[(A, Package.Parsed)],
      ifs: List[Package.Interface],
      compileOptions: CompileOptions
  )(implicit cpuEC: Par.EC): Ior[NonEmptyList[PackageError], Inferred] = {
    import Par.F
    Par.await(
      resolveThenInfer[F, A](
        ps,
        ifs,
        compileOptions,
        InferCache.noop[F],
        InferPhases.default
      )
    )
  }

  def typeCheckParsed[A: Show](
      packs: NonEmptyList[((A, LocationMap), Package.Parsed)],
      ifs: List[Package.Interface],
      predefKey: A,
      compileOptions: CompileOptions
  )(implicit
      cpuEC: Par.EC
  ): Ior[NonEmptyList[PackageError], PackageMap.Inferred] = {
    import Par.F
    Par.await(
      typeCheckParsed[F, A](
        packs,
        ifs,
        predefKey,
        compileOptions,
        InferCache.noop[F],
        InferPhases.default
      )
    )
  }

  private def internalPredefCompileOptions(
      mode: CompileOptions.Mode
  ): CompileOptions =
    mode match {
      case CompileOptions.Mode.Emit          => CompileOptions.Default
      case CompileOptions.Mode.TypeCheckOnly => CompileOptions.TypeCheckOnly
    }

  private def compilePredefForMode(
      mode: CompileOptions.Mode
  ): Package.Inferred =
    Par.noParallelism {
      val inferred = PackageMap
        .resolveThenInfer(
          ((), Package.predefPackageForMode(mode)) :: Nil,
          Nil,
          internalPredefCompileOptions(mode)
        )
        .strictToValidated

      inferred match {
        case Validated.Valid(v) =>
          v.toMap
            .get(PackageName.PredefName)
            .expect("internal error: predef package not found after compilation")
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

  private lazy val predefCompiledEmit: Package.Inferred =
    compilePredefForMode(CompileOptions.Mode.Emit)
  private lazy val predefCompiledTypeCheckOnly: Package.Inferred =
    compilePredefForMode(CompileOptions.Mode.TypeCheckOnly)

  // Compile mode changes the internal predef exports (`todo` in type-check
  // mode), so mode must be part of predef cache identity.
  def predefCompiledForMode(mode: CompileOptions.Mode): Package.Inferred =
    mode match {
      case CompileOptions.Mode.Emit          => predefCompiledEmit
      case CompileOptions.Mode.TypeCheckOnly => predefCompiledTypeCheckOnly
    }

  /** Backward compatible runtime predef handle.
    */
  def predefCompiled: Package.Inferred = predefCompiledEmit

  private def predefImportsFromExports(
      exports: List[ExportedName[Referant[Kind.Arg]]]
  ): Import[PackageName, Unit] = {
    val predefImportList = exports
      .map(_.name)
      .distinct
      .sorted
      .map(ImportedName.OriginalName(_, ()))
    Import(PackageName.PredefName, NonEmptyList.fromList(predefImportList).get)
  }

  private lazy val predefImportsEmit: Import[PackageName, Unit] =
    predefImportsFromExports(predefCompiledEmit.exports)
  private lazy val predefImportsTypeCheckOnly: Import[PackageName, Unit] =
    predefImportsFromExports(predefCompiledTypeCheckOnly.exports)

  private def predefImportsForMode(
      mode: CompileOptions.Mode
  ): Import[PackageName, Unit] =
    mode match {
      case CompileOptions.Mode.Emit          => predefImportsEmit
      case CompileOptions.Mode.TypeCheckOnly => predefImportsTypeCheckOnly
    }

  private def withPredefImportsA[A](
      ps: List[(A, Package.Parsed)],
      predefImports: Import[PackageName, Unit]
  ): List[(A, Package.Parsed)] =
    ps.map { case (a, p) => (a, p.withImport(predefImports)) }

  def withPredef(ps: List[Package.Parsed]): List[Package.Parsed] =
    withPredef(ps, CompileOptions.Mode.Emit)

  def withPredef(
      ps: List[Package.Parsed],
      mode: CompileOptions.Mode
  ): List[Package.Parsed] =
    Package.predefPackageForMode(mode) :: ps.map(_.withImport(
      predefImportsForMode(mode)
    ))

  def withPredefA[A](
      predefA: A,
      ps: List[(A, Package.Parsed)]
  ): List[(A, Package.Parsed)] =
    withPredefA(predefA, ps, CompileOptions.Mode.Emit)

  def withPredefA[A](
      predefA: A,
      ps: List[(A, Package.Parsed)],
      mode: CompileOptions.Mode
  ): List[(A, Package.Parsed)] =
    (predefA, Package.predefPackageForMode(mode)) :: withPredefImportsA(
      ps,
      predefImportsForMode(mode)
    )

}
