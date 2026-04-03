package dev.bosatsu

import dev.bosatsu.graph.{CanPromise, Dag, Memoize, Toposort}
import cats.{Applicative, Foldable, Monad, Parallel, Show}
import cats.data.{
  Ior,
  IorT,
  NonEmptyChain,
  NonEmptyList,
  NonEmptyMap,
  Validated,
  ValidatedNel
}
import scala.collection.immutable.SortedMap

import Identifier.Constructor
import IorMethods.IorExtension

import rankn.{DataRepr, TypeEnv}
import cats.implicits._
import dev.bosatsu.cache.{CompileCache, InferCache, InferPhases}
import dev.bosatsu.hashing.{Algo, HashValue}
import scala.util.hashing.MurmurHash3

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
      exposes: List[List[PackageName]],
      sourceHash: HashValue[Algo.Blake3],
      loadParsed: F[Package.Parsed]
  ) {
    override lazy val hashCode: Int =
      MurmurHash3.caseClassHash(this)
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
        exposes = parsed.exposes,
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
  type Compiled = Typed[Region]

  /** This builds a DAG of actual packages where on Import the PackageName have
    * been replaced by the Either a Package.Interface (which gives exports only)
    * or this same recursive structure.
    */
  def resolvePackages[A, B, C](
      map: PackageMap[PackageName, A, B, C],
      ifs: List[Package.Interface]
  ): ValidatedNel[PackageError, MapF3[A, B, C]] = {
    val packagesByName = map.toMap
    val interfaceMap = ifs.iterator.map(iface => (iface.name, iface)).toMap

    def getPackage(
        i: Import[PackageName, A],
        from: PackageName
    ): ValidatedNel[PackageError, Import[
      Either[Package.Interface, PackageName],
      A
    ]] =
      packagesByName.get(i.pack) match {
        case Some(_)    => Validated.valid(Import(Right(i.pack), i.items))
        case None       =>
          interfaceMap.get(i.pack) match {
            case Some(iface) =>
              Validated.valid(Import(Left(iface), i.items))
            case None =>
              Validated.invalidNel(
                PackageError.UnknownImportPackage(i.pack, from)
              )
          }
      }

    type PackageFix = Package[FixPackage[A, B, C], A, B, C]
    type ErrorOr[A] = Either[NonEmptyChain[PackageError], A]
    type Resolve[A] = List[PackageName] => ErrorOr[A]

    // Explicit loops avoid building the deep Kleisli/Eval chains that overflow
    // native-image stacks on the Zafu package graph.
    def accumulateList[X, Y](items: List[X])(fn: X => ErrorOr[Y]): ErrorOr[
      List[Y]
    ] = {
      var remaining = items
      var revItems = List.empty[Y]
      var errors: Nullable[NonEmptyChain[PackageError]] = Nullable.empty

      while (remaining.nonEmpty) {
        fn(remaining.head) match {
          case Right(item) =>
            if (errors.isNull) {
              revItems = item :: revItems
            }
          case Left(errs) =>
            errors = Nullable(errors.fold(errs)(_ ++ errs))
        }
        remaining = remaining.tail
      }

      errors.fold(Right(revItems.reverse): ErrorOr[List[Y]])(Left(_))
    }

    // We still thread the current import path through the resolver so circular
    // dependency errors keep reporting the cycle itself rather than just the
    // repeated package name.
    val step: PackageName => Resolve[PackageFix] =
      Memoize.memoizeDagHashed[PackageName, Resolve[PackageFix]] {
        (pname, rec) =>
          (path: List[PackageName]) => {
            val p = packagesByName.get(pname).expect {
              s"invariant violation: missing package definition for $pname"
            }

            path match {
              case nonE @ (h :: tail) if nonE.contains(pname) =>
                Left(
                  NonEmptyChain.one(
                    PackageError.CircularDependency(pname, NonEmptyList(h, tail))
                  )
                )
              case _ =>
                val depsOr =
                  accumulateList(p.imports) { i =>
                    getPackage(i, pname).toEither.leftMap(NonEmptyChain.fromNonEmptyList)
                  }

                depsOr.flatMap { deps =>
                  accumulateList(deps) { i =>
                    i.pack match {
                      case Right(depName) =>
                        rec(depName)(pname :: path).map { dep =>
                          Import(Package.fix[A, B, C](Right(dep)), i.items)
                        }
                      case Left(iface) =>
                        Right(
                          Import(Package.fix[A, B, C](Left(iface)), i.items)
                        )
                    }
                  }.map { imports =>
                    Package(p.name, imports, p.exports, p.program, p.exposes)
                  }
                }
            }
          }
      }

    type M = SortedMap[PackageName, PackageFix]
    val m: ErrorOr[M] =
      accumulateList(packagesByName.keys.toList) { pname =>
        step(pname)(Nil).map(pname -> _)
      }.map(SortedMap.from(_))

    m.leftMap(_.toNonEmptyList)
      .leftMap(normalizeCircularDependencyErrors)
      .map(PackageMap(_))
      .toValidated
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
        Package(
          source.packageName,
          source.imports,
          source.exports,
          (source, imap),
          source.exposes
        )
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
      cache: InferCache[F, CompileCache.GenerateKeyInput, Package.Compiled],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], Compiled]] = {
    type SourceProgram = (SourceUnit[F, A], ImportMap[PackageName, Unit])
    type ResolvedFix = FixPackage[Unit, Unit, SourceProgram]
    type ResolvedU = Package[
      ResolvedFix,
      Unit,
      Unit,
      SourceProgram
    ]
    type ErrorOr[A1] = Ior[NonEmptyList[PackageError], A1]

    final case class CompiledPack(
        compiled: Package.Compiled,
        depInterface: Package.Interface
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

    def toCompiledPack(compiled: Package.Compiled): CompiledPack =
      CompiledPack(compiled, phases.dependencyInterface(compiled))

    val inferPack: PackageName => F[ErrorOr[CompiledPack]] =
      Memoize.memoizeDag[F, PackageName, ErrorOr[CompiledPack]] {
        case (packName, recurse) =>
          val pack = resolvedByName.get(packName).expect {
            s"invariant violation: missing resolved package for $packName"
          }
          pack match {
            case Package(nm, imports, exports, (source, imps), exposes) =>
              val depResultsF: F[ErrorOr[SortedMap[PackageName, CompiledPack]]] =
                imports.foldLeft(
                  Monad[F].pure(
                    Ior.right[NonEmptyList[PackageError], SortedMap[
                      PackageName,
                      CompiledPack
                    ]](SortedMap.empty)
                  )
                ) { (accF, imp) =>
                  Package.unfix(imp.pack) match {
                    case Left(_)        => accF
                    case Right(depPack) =>
                      val nextF: F[ErrorOr[(PackageName, CompiledPack)]] =
                        recurse(depPack.name)
                          .map(_.map(dep => dep.compiled.name -> dep))
                      (accF, nextF).parMapN { (acc, next) =>
                        (acc, next).parMapN { (deps, dep) =>
                          deps.updated(dep._1, dep._2)
                        }
                      }
                  }
                }

              IorT(depResultsF)
                .flatMap { depResults =>
                  def depPackResult(depPack: ResolvedU): ErrorOr[Package.Compiled] =
                    Ior.right(depResults.get(depPack.name).expect {
                      s"invariant violation: missing dependency result for ${depPack.name}"
                    }.compiled)

                  def depPackMeta(
                      depPack: ResolvedU
                  ): ErrorOr[CompiledPack] =
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
                              inferredDep.compiled.name -> inferredDep.depInterface
                            }
                          case Left(iface)    =>
                            Ior.right(iface.name -> iface)
                        }

                      (acc, next).parMapN { (ifaces, iface) =>
                        ifaces.updated(iface._1, iface._2)
                      }
                    }

                  def inferOnMiss(
                      depIfaces: SortedMap[PackageName, Package.Interface]
                  ): F[ErrorOr[Package.Compiled]] = {
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
                      val rec1: ResolvedU => ErrorOr[Package.Compiled] =
                        depPack => depPackResult(depPack)
                      resolvedImports.parTraverse {
                        (fixpack: ResolvedFix, item: ImportedName[Unit]) =>
                          importResolvedName[[A1] =>> ErrorOr[A1], Region](
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
                              program,
                              exposes
                            )
                            finished =
                              phases.finishPackage(asm, depIfaces, compileOptions)
                          } yield Package.toCompiled(finished)
                        }
                      }
                    }
                  }

                  import IorT.{fromIor, liftF}

                  (for {
                    depIfaces <- fromIor[F](depInterfaces)
                    key <- liftF(
                      cache.generateKey(
                        (
                          nm,
                          source.sourceHash,
                          depIfaces,
                          compileOptions,
                          CompileCache.compilerIdentity,
                          phases.id
                        )
                      )
                    )
                    getRes <- liftF(cache.get(key))
                    res <- getRes match {
                      case Some(hit) =>
                        IorT.rightT[F, NonEmptyList[PackageError]](hit)
                      case None      =>
                        for {
                          compiled <- IorT(inferOnMiss(depIfaces))
                          _ <- liftF(cache.put(key, compiled))
                        } yield compiled
                    }
                    compiledPack = toCompiledPack(res)
                  } yield compiledPack)
                }
                .value
          }
      }

    val allResults: F[SortedMap[PackageName, ErrorOr[CompiledPack]]] =
      resolvedByName.keys.toList
        .parTraverse { packName =>
          inferPack(packName).map(packName -> _)
        }
        .map(SortedMap.from(_))

    def dedupeErrors[A1](res: ErrorOr[A1]): ErrorOr[A1] =
      res.leftMap { errs =>
        NonEmptyList.fromListUnsafe(errs.toList.distinct)
      }

    allResults.map { resultMap =>
      val missingKeys = resolvedByName.keySet.diff(resultMap.keySet).toList.sorted
      if (missingKeys.nonEmpty) {
        sys.error(
          s"invariant violation: missing inference results for: ${missingKeys.mkString(", ")}"
        )
      } else {
        val sequenced =
          resultMap.traverse {
            case Ior.Left(errs)          =>
              Ior.both(errs, Option.empty[CompiledPack])
            case Ior.Right(compiledPack) =>
              Ior.right(Some(compiledPack))
            case Ior.Both(errs, compiledPack) =>
              Ior.both(errs, Some(compiledPack))
          }

        dedupeErrors(sequenced)
          .map { entries =>
            PackageMap(
              SortedMap.from(
                entries.iterator.collect { case (name, Some(compiledPack)) =>
                  name -> compiledPack.compiled
                }
              )
            )
          } match {
          case Ior.Both(errs, compiled) if compiled.toMap.isEmpty =>
            Ior.left(errs)
          case other                                              =>
            other
        }
      }
    }
  }

  private def resolveThenInferSourceUnits[F[_]: Monad: Parallel: CanPromise, A: Show](
      ps: List[SourceUnit[F, A]],
      ifs: List[Package.Interface],
      compileOptions: CompileOptions,
      cache: InferCache[F, CompileCache.GenerateKeyInput, Package.Compiled],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], Compiled]] =
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

  private[bosatsu] final case class EffectiveSourceUnits[F[_], A](
      sourceUnits: List[SourceUnit[F, A]],
      usesInternalPredefSource: Boolean
  )

  // A user-supplied `Bosatsu/Predef` interface suppresses the internal predef
  // source, but we still inject the same implicit import surface either way.
  private[bosatsu] def effectivePredefSources[F[_]: Applicative, A](
      sources: NonEmptyList[SourceUnit[F, A]],
      ifs: List[Package.Interface],
      predefKey: A,
      mode: CompileOptions.Mode
  ): EffectiveSourceUnits[F, A] = {
    val predefIface = ifs.find(_.name == PackageName.PredefName)
    val withPredefImports =
      withPredefImportsSourceUnits(
        sources.toList,
        predefIface.fold(predefImportsForMode(mode))(iface =>
          predefImportsFromExports(iface.exports)
        )
      )

    predefIface match {
      case None       =>
        EffectiveSourceUnits(
          SourceUnit.predef(predefKey, mode) :: withPredefImports,
          usesInternalPredefSource = true
        )
      case Some(_)    =>
        EffectiveSourceUnits(
          withPredefImports,
          usesInternalPredefSource = false
        )
    }
  }

  def typeCheckSources[F[_]: Monad: Parallel: CanPromise, A: Show](
      sources: NonEmptyList[SourceUnit[F, A]],
      ifs: List[Package.Interface],
      predefKey: A,
      compileOptions: CompileOptions,
      cache: InferCache[F, CompileCache.GenerateKeyInput, Package.Compiled],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], PackageMap.Compiled]] =
    PackageMap.resolveThenInferSourceUnits[F, A](
      effectivePredefSources(
        sources,
        ifs,
        predefKey,
        compileOptions.mode
      ).sourceUnits,
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
      cache: InferCache[F, CompileCache.GenerateKeyInput, Package.Compiled],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], PackageMap.Compiled]] =
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
      cache: InferCache[F, CompileCache.GenerateKeyInput, Package.Compiled],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], Compiled]] =
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
  )(implicit cpuEC: Par.EC): Ior[NonEmptyList[PackageError], Compiled] = {
    import Par.F
    Par.await(
      resolveThenInfer[F, A](
        ps,
        ifs,
        compileOptions,
        InferCache.noop[F, CompileCache.GenerateKeyInput, Package.Compiled],
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
  ): Ior[NonEmptyList[PackageError], PackageMap.Compiled] = {
    import Par.F
    Par.await(
      typeCheckParsed[F, A](
        packs,
        ifs,
        predefKey,
        compileOptions,
        InferCache.noop[F, CompileCache.GenerateKeyInput, Package.Compiled],
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

  private def compilePredefInferredForMode(
      mode: CompileOptions.Mode
  ): Package.Inferred =
    Par.noParallelism {
      val parsed = Package.predefPackageForMode(mode)
      val inferred = (
        for {
          (_, program) <- Package.inferBodyUnopt(
            parsed.name,
            Nil,
            parsed.exports,
            parsed.program
          )
          assembled <- PackageCustoms.assemble(
            parsed.name,
            Nil,
            ImportMap.empty,
            parsed.exports,
            program,
            parsed.exposes
          )
        } yield InferPhases.default.finishPackage(
          assembled,
          SortedMap.empty,
          internalPredefCompileOptions(mode)
        )
      ).strictToValidated

      inferred match {
        case Validated.Valid(v) =>
          v
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

  private lazy val predefInferredEmit: Package.Inferred =
    compilePredefInferredForMode(CompileOptions.Mode.Emit)
  private lazy val predefInferredTypeCheckOnly: Package.Inferred =
    compilePredefInferredForMode(CompileOptions.Mode.TypeCheckOnly)
  private lazy val predefCompiledEmit: Package.Compiled =
    Package.toCompiled(predefInferredEmit)
  private lazy val predefCompiledTypeCheckOnly: Package.Compiled =
    Package.toCompiled(predefInferredTypeCheckOnly)

  // Compile mode changes the internal predef exports (`todo` in type-check
  // mode), so mode must be part of predef cache identity.
  def predefCompiledForMode(mode: CompileOptions.Mode): Package.Compiled =
    mode match {
      case CompileOptions.Mode.Emit          => predefCompiledEmit
      case CompileOptions.Mode.TypeCheckOnly => predefCompiledTypeCheckOnly
    }

  /** Backward compatible runtime predef handle.
    */
  def predefCompiled: Package.Compiled = predefCompiledEmit

  def predefInferredForMode(mode: CompileOptions.Mode): Package.Inferred =
    mode match {
      case CompileOptions.Mode.Emit          => predefInferredEmit
      case CompileOptions.Mode.TypeCheckOnly => predefInferredTypeCheckOnly
    }

  def predefInferred: Package.Inferred = predefInferredEmit

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
