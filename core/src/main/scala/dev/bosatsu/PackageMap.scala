package dev.bosatsu

import dev.bosatsu.graph.{CanPromise, Dag, Memoize, Toposort}
import cats.{Foldable, Monad, Parallel, Show}
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
import dev.bosatsu.cache.{CompileCache, InferCache, InferPhases}

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
          val leftPredef = p1 == PackageName.PredefName
          val rightPredef = p2 == PackageName.PredefName

          if (leftPredef) {
            if (rightPredef) {
              // Both are predef, if one is renamed, choose that, else error
              val r1 = i1.isRenamed
              val r2 = i2.isRenamed
              if (r1 && !r2) ImportMap.Unify.Left
              else if (!r1 && r2) ImportMap.Unify.Right
              else if ((i1 === i2) && !r1) {
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
      ps: Resolved,
      compileOptions: CompileOptions
  )(implicit cpuEC: Par.EC): Ior[NonEmptyList[PackageError], Inferred] = {
    import Par.F
    Par.await(
      inferAll[F](
        ps,
        compileOptions,
        InferCache.noop[F],
        InferPhases.default
      )
    )
  }

  def inferAll[F[_]: Monad: Parallel: CanPromise](
      ps: Resolved,
      compileOptions: CompileOptions,
      cache: InferCache[F],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], Inferred]] = {
    // This is unfixed resolved
    type ResolvedU = Package[
      FixPackage[Unit, Unit, (List[Statement], ImportMap[PackageName, Unit])],
      Unit,
      Unit,
      (List[Statement], ImportMap[PackageName, Unit])
    ]
    type ErrorOr[A] = Ior[NonEmptyList[PackageError], A]
    type CacheDepHash = cache.DepHash

    final case class InferredPack[H](
        inferred: Package.Inferred,
        depInterface: Package.Interface,
        depInterfaceHash: H
    )

    val resolvedByName: SortedMap[PackageName, ResolvedU] = ps.toMap

    def sourceDeps(pack: ResolvedU): List[PackageName] =
      pack.imports.iterator
        .collect {
          case imp if Package.unfix(imp.pack).isRight =>
            imp.pack.name
        }
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
            case Package(nm, imports, exports, (stmt, imps)) =>
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

                  val parsedForKey: Package.Parsed =
                    Package(
                      nm,
                      imports.map(i => Import(i.pack.name, i.items)),
                      exports,
                      stmt
                    )

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
                    val nameToRes: Map[PackageName, Package.Resolved] = imports.iterator
                      .map { i =>
                        val resolved: Package.Resolved = i.pack
                        (resolved.name, resolved)
                      }
                      .toMap

                    val resolvedImports: ImportMap[Package.Resolved, Unit] =
                      imps.traverse[cats.Id, Package.Resolved, Unit] { (p, i) =>
                        // the Map.apply below should be safe because the imps
                        // are aligned with imports
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
                        (fixpack: Package.Resolved, item: ImportedName[Unit]) =>
                          fixpack
                            .importName[[A] =>> ErrorOr[A], Declaration](
                              nm,
                              item
                            )(rec1)
                            .flatMap { either =>
                              Ior.fromEither(either.left.map(NonEmptyList.one(_)))
                            }
                      }
                    }

                    inferImports.flatTraverse { impMap =>
                      val ilist = impMap.toList(using Package.orderByName)
                      // we need to use compute here because this can be a bit
                      // heavy and in cats.effect.IO it should be on the blocking
                      // threadpool.
                      summon[CanPromise[F]].compute {
                        for {
                          (_, program) <- Package.inferBodyUnopt(
                            nm,
                            ilist,
                            exports,
                            stmt
                          )
                          asm <- PackageCustoms.assemble(nm, ilist, impMap, exports, program)
                        } yield phases.finishPackage(asm, depIfaces, compileOptions)
                      }
                    }
                  }

                  import IorT.{fromIor, liftF}

                  (for {
                    depIfaceHashes <- IorT(depInterfaceHashesF)
                    key <- liftF(
                      cache.generateKey(
                        parsedForKey,
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

    def dedupeErrors[A](res: ErrorOr[A]): ErrorOr[A] =
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
        dedupeErrors(resultMap.sequence.map { inferredByName =>
          PackageMap(
            SortedMap.from(
              inferredByName.iterator.map { case (name, inferredPack) =>
                name -> inferredPack.inferred
              }
            )
          )
        })
      }
    }
  }

  def resolveThenInfer[F[_]: Monad: Parallel: CanPromise, A: Show](
      ps: List[(A, Package.Parsed)],
      ifs: List[Package.Interface],
      compileOptions: CompileOptions,
      cache: InferCache[F],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], Inferred]] =
    IorT(
      summon[CanPromise[F]].compute {
        resolveAll(ps, ifs)
      }
    )
      .flatMap(resolved => IorT(inferAll(resolved, compileOptions, cache, phases)))
      .value

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

  def buildSourceMap[F[_]: Foldable, A: Show](
      parsedFiles: F[((A, LocationMap), Package.Parsed)]
  ): Map[PackageName, (LocationMap, String)] =
    parsedFiles.foldLeft(Map.empty[PackageName, (LocationMap, String)]) {
      case (map, ((path, lm), pack)) =>
        map.updated(pack.name, (lm, path.show))
    }

  /** typecheck a list of packages given a list of interface dependencies
    *
    * @param packs
    *   a list of parsed packages, along with a key A to tag the source
    * @param ifs
    *   the interfaces we are compiling against. If Bosatsu.Predef is not in
    *   this list, the default is added
    */
  private def withEffectivePredef[A](
      packs: NonEmptyList[((A, LocationMap), Package.Parsed)],
      ifs: List[Package.Interface],
      predefKey: A,
      mode: CompileOptions.Mode
  ): List[(A, Package.Parsed)] = {
    val predefIface = ifs.find(_.name == PackageName.PredefName)
    // if we have passed in a user supplied predef, don't use the internal one
    val useInternalPredef = predefIface.isEmpty

    val parsed =
      if (useInternalPredef)
        withPredefA[(A, LocationMap)](
          (predefKey, LocationMap("")),
          packs.toList,
          mode
        )
      else {
        val predefImports = predefIface match {
          case Some(iface) => predefImportsFromExports(iface.exports)
          case None        =>
            // This should be unreachable because useInternalPredef is false.
            predefImportsForMode(mode)
        }
        withPredefImportsA[(A, LocationMap)](packs.toList, predefImports)
      }

    parsed.map { case ((a, _), p) => (a, p) }
  }

  def typeCheckParsed[F[_]: Monad: Parallel: CanPromise, A: Show](
      packs: NonEmptyList[((A, LocationMap), Package.Parsed)],
      ifs: List[Package.Interface],
      predefKey: A,
      compileOptions: CompileOptions,
      cache: InferCache[F],
      phases: InferPhases
  ): F[Ior[NonEmptyList[PackageError], PackageMap.Inferred]] =
    PackageMap
      .resolveThenInfer[F, A](
        withEffectivePredef(
          packs,
          ifs,
          predefKey,
          compileOptions.mode
        ),
        ifs,
        compileOptions,
        cache,
        phases
      )

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
