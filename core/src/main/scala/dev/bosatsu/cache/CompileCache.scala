package dev.bosatsu.cache

import _root_.bosatsu.{TypedAst => proto}
import cats.syntax.all._
import dev.bosatsu.hashing.{Algo, Hashed, HashValue}
import dev.bosatsu.{
  BuildInfo,
  CompileOptions,
  Package,
  PackageName,
  PlatformIO,
  ProtoConverter,
  Region,
  Statement
}
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import org.typelevel.paiges.{Doc, Document}
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success, Try}

object CompileCache {
  private val schemaVersion = 1
  private val sentinelRegion = Region(0, 0)
  private val utf8 = StandardCharsets.UTF_8
  private val blake3 = Algo.blake3Algo

  def compilerIdentity: String = {
    val gitSha = BuildInfo.gitHeadCommit.getOrElse("nogit")
    s"${BuildInfo.version}:$gitSha:${BuildInfo.scalaVersion}"
  }

  def filesystem[F[_], P](
      cacheDir: P,
      platformIO: PlatformIO[F, P]
  ): InferCache[F] { type Key = FsKey; type DepHash = HashValue[Algo.Blake3] } =
    new FilesystemCache(cacheDir, platformIO)

  def sourceExprHash(pack: Package.Parsed): HashValue[Algo.Blake3] = {
    val normalizedStatements = pack.program.collect {
      case _: Statement.PaddingStatement => None
      case _: Statement.Comment          => None
      case stmt                          => Some(stmt.replaceRegions(sentinelRegion))
    }.flatten
    val normalized = pack.copy(program = normalizedStatements)
    val serialized = Document[Package.Parsed].document(normalized).render(200)
    hashUtf8(serialized)
  }

  def interfaceHash(iface: Package.Interface): Try[HashValue[Algo.Blake3]] =
    ProtoConverter
      .interfaceToProto(iface)
      .map(p => Algo.hashBytes[Algo.Blake3](p.toByteArray))

  def keyHashHex(key: FsKey): String =
    keyHashValue(key).hex

  def outputHashHex(pack: Package.Inferred): Option[String] =
    outputHashValue(pack).toOption.map(_.hash.hex)

  private[cache] def keyHashValue(key: FsKey): HashValue[Algo.Blake3] = {
    val deps = key.depInterfaceHashes
      .map { case (pn, hash) =>
        s"${pn.asString}=${hash.toIdent(using blake3)}"
      }
      .mkString("\n")
    val payload =
      s"""schema:${key.schemaVersion}
         |package:${key.packageName.asString}
         |mode:${key.compileOptions.mode}
         |optimize:${key.compileOptions.optimize}
         |compiler:${key.compilerIdentity}
         |phase:${key.phaseIdentity}
         |source:${key.sourceHash.toIdent(using blake3)}
         |deps:
         |$deps
         |""".stripMargin
    hashUtf8(payload)
  }

  private[cache] def outputHashValue(
      pack: Package.Inferred
  ): Try[Hashed[Algo.Blake3, Array[Byte]]] =
    ProtoConverter
      .packagesToProto(pack :: Nil)
      .map { p =>
        val bytes = p.toByteArray
        Hashed(Algo.hashBytes[Algo.Blake3](bytes), bytes)
      }

  private def hashUtf8(str: String): HashValue[Algo.Blake3] =
    Algo.hashBytes[Algo.Blake3](str.getBytes(utf8))

  private final class FilesystemCache[F[_], P](
      cacheDir: P,
      platformIO: PlatformIO[F, P]
  ) extends InferCache[F] {
    type Key = FsKey
    type DepHash = HashValue[Algo.Blake3]

    import platformIO.moduleIOMonad

    private val statsEnabled = InferCache.statsEnabled
    private val cacheDirLabel = platformIO.pathToString(cacheDir)
    private val keyGenCalls = new AtomicLong(0L)
    private val keyGenFailures = new AtomicLong(0L)
    private val interfaceMemoHits = new AtomicLong(0L)
    private val interfaceMemoMisses = new AtomicLong(0L)
    private val dependencyHashCalls = new AtomicLong(0L)
    private val getCalls = new AtomicLong(0L)
    private val getHits = new AtomicLong(0L)
    private val getMisses = new AtomicLong(0L)
    private val getLinkReadErrors = new AtomicLong(0L)
    private val getLinkParseMisses = new AtomicLong(0L)
    private val getCasReadErrors = new AtomicLong(0L)
    private val getCasDecodeMisses = new AtomicLong(0L)
    private val firstGetLinkReadError = new AtomicReference[String](null)
    private val firstGetCasReadError = new AtomicReference[String](null)
    private val putCalls = new AtomicLong(0L)
    private val putEncodeFailures = new AtomicLong(0L)
    private val putCasAlreadyExists = new AtomicLong(0L)
    private val putCasWrites = new AtomicLong(0L)
    private val putCasWriteErrors = new AtomicLong(0L)
    private val putLinkWrites = new AtomicLong(0L)
    private val putLinkWriteErrors = new AtomicLong(0L)

    private inline def statsUpdate(inline fn: => Unit): Unit =
      if (statsEnabled) fn

    private def ratioPct(numerator: Long, denominator: Long): String =
      if (denominator == 0L) "n/a"
      else f"${(numerator.toDouble * 100.0) / denominator.toDouble}%.2f%%"

    private final class RefKey[A <: AnyRef](val ref: A) {
      override def equals(that: Any): Boolean =
        that match {
          case other: RefKey[_] =>
            (ref.asInstanceOf[AnyRef] eq other.ref.asInstanceOf[AnyRef])
          case _                => false
        }

      override def hashCode(): Int =
        System.identityHashCode(ref)
    }

    private val interfaceHashMemo =
      new ConcurrentHashMap[
        RefKey[Package.Interface],
        Try[HashValue[Algo.Blake3]]
      ]()
    // inferAll memoizes each inferred package and computes that package's
    // dependency interface hash once per run. We still hash "Left(iface)"
    // dependencies on each importer edge, so this per-cache memo avoids
    // repeated interface proto serialization for those shared externals.
    private val interfaceByHashHex =
      new ConcurrentHashMap[String, Package.Interface]()

    private def memoizedInterfaceHash(
        iface: Package.Interface
    ): Try[HashValue[Algo.Blake3]] = {
      val key = new RefKey(iface)
      val cached = interfaceHashMemo.get(key)
      if (cached != null) {
        statsUpdate { interfaceMemoHits.incrementAndGet(); () }
        cached
      } else {
        val computed = interfaceHash(iface)
        val raced = interfaceHashMemo.putIfAbsent(key, computed)
        if (raced == null) {
          statsUpdate { interfaceMemoMisses.incrementAndGet(); () }
          computed
        } else {
          statsUpdate { interfaceMemoHits.incrementAndGet(); () }
          raced
        }
      }
    }

    override def dependencyHash(interface: Package.Interface): F[DepHash] = {
      statsUpdate { dependencyHashCalls.incrementAndGet(); () }
      platformIO.canPromiseF
        .compute {
          memoizedInterfaceHash(interface)
        }
        .flatMap(moduleIOMonad.fromTry(_))
        .map { hash =>
          interfaceByHashHex.putIfAbsent(hash.hex, interface)
          hash
        }
    }

    private def keyPath(hash: HashValue[Algo.Blake3]): P =
      platformIO.resolve(
        cacheDir,
        "keys" :: blake3.name :: hash.hex.take(2) :: hash.hex.drop(2) :: Nil
      )

    private def casPath(hash: HashValue[Algo.Blake3]): P =
      platformIO.resolve(
        cacheDir,
        "cas" :: blake3.name :: hash.hex.take(2) ::
          s"${hash.hex.drop(2)}.bosatsu_package" :: Nil
      )

    private def parseHashIdent(str: String): Option[HashValue[Algo.Blake3]] =
      Algo.parseHashValue(blake3).parseAll(str.trim).toOption

    private inline def onError[A](
        fa: F[A],
        inline fallback: => A,
        inline onErr: Throwable => Unit
    ): F[A] =
      moduleIOMonad.handleError(fa) { err =>
        onErr(err)
        fallback
      }

    override def generateKey(
        packageName: PackageName,
        sourceHash: HashValue[Algo.Blake3],
        depInterfaceHashes: SortedMap[PackageName, DepHash],
        compileOptions: CompileOptions,
        compilerIdentity: String,
        phaseIdentity: String
    ): F[Key] = {
      statsUpdate { keyGenCalls.incrementAndGet(); () }
      platformIO.canPromiseF.compute {
        val depInterfacesBuilder =
          SortedMap.newBuilder[PackageName, Package.Interface]
        val depHashIter = depInterfaceHashes.iterator

        var failure: Option[Throwable] = None
        while (depHashIter.hasNext && failure.isEmpty) {
          val (name, hash) = depHashIter.next()
          val iface = interfaceByHashHex.get(hash.hex)
          if (iface eq null) {
            failure = Some(
              new IllegalStateException(
                s"missing dependency interface for ${name.asString} (${hash.hex}); dependencyHash must be called before generateKey"
              )
            )
          } else {
            depInterfacesBuilder += ((name, iface))
          }
        }

        failure match {
          case None      =>
            Success(
              FsKey(
                packageName = packageName,
                compileOptions = compileOptions,
                compilerIdentity = compilerIdentity,
                phaseIdentity = phaseIdentity,
                sourceHash = sourceHash,
                depInterfaceHashes = depInterfaceHashes,
                depInterfaces = depInterfacesBuilder.result(),
                schemaVersion = schemaVersion
              )
            )
          case Some(err) =>
            statsUpdate { keyGenFailures.incrementAndGet(); () }
            Failure(err)
        }
      }
      .flatMap(moduleIOMonad.fromTry(_))
    }

    def get(key: Key): F[Option[Package.Inferred]] = {
      statsUpdate { getCalls.incrementAndGet(); () }
      val keyHash = keyHashValue(key)
      val linkPath = keyPath(keyHash)

      val readLink =
        platformIO.readUtf8(linkPath).map { raw =>
          parseHashIdent(raw) match {
            case some @ Some(_) => some
            case None           =>
              statsUpdate { getLinkParseMisses.incrementAndGet(); () }
              None
          }
        }

      onError(
        readLink,
        None,
        err =>
          statsUpdate {
            getLinkReadErrors.incrementAndGet()
            val _ = firstGetLinkReadError.compareAndSet(
              null,
                s"${err.getClass.getName}:${Option(err.getMessage).getOrElse("")}"
            )
            ()
          }
      ).flatMap {
        case None             =>
          statsUpdate { getMisses.incrementAndGet(); () }
          moduleIOMonad.pure(None)
        case Some(outputHash) =>
          val packagePath = casPath(outputHash)
          val depIfaces = key.depInterfaces.valuesIterator.toList
          val read = platformIO.readBytes(packagePath).flatMap { bytes =>
            moduleIOMonad.fromTry {
              for {
                protoPackages <- Try(proto.Packages.parseFrom(bytes))
                decoded <- ProtoConverter
                  .packagesFromProto(Nil, protoPackages.packages, depIfaces)
              } yield decoded._2
            }.map {
              case pack :: Nil if pack.name == key.packageName =>
                // Serialized package artifacts are tag-erased to Unit.
                Some(pack.asInstanceOf[Package.Inferred])
              case _ =>
                statsUpdate { getCasDecodeMisses.incrementAndGet(); () }
                None
            }
          }
          onError(
            read,
            None,
            err =>
              statsUpdate {
                getCasReadErrors.incrementAndGet()
                val _ = firstGetCasReadError.compareAndSet(
                  null,
                    s"${err.getClass.getName}:${Option(err.getMessage).getOrElse("")}"
                )
                ()
              }
          )
            .map {
              case some @ Some(_) =>
                statsUpdate { getHits.incrementAndGet(); () }
                some
              case None           =>
                statsUpdate { getMisses.incrementAndGet(); () }
                None
            }
      }
    }

    def put(key: Key, value: Package.Inferred): F[Unit] =
      outputHashValue(value) match {
        case Failure(_)            =>
          statsUpdate {
            putCalls.incrementAndGet()
            putEncodeFailures.incrementAndGet()
            ()
          }
          moduleIOMonad.unit
        case Success(hashedOutput) =>
          statsUpdate { putCalls.incrementAndGet(); () }
          val outputHash = hashedOutput.hash
          val packagePath = casPath(outputHash)

          val writeCas: F[Boolean] =
            onError(
              platformIO.fileExists(packagePath).flatMap {
                case true  =>
                  statsUpdate { putCasAlreadyExists.incrementAndGet(); () }
                  moduleIOMonad.pure(true)
                case false =>
                  platformIO.writeBytes(packagePath, hashedOutput.arg).as {
                    statsUpdate { putCasWrites.incrementAndGet(); () }
                    true
                  }
              },
              false,
              _ => statsUpdate { putCasWriteErrors.incrementAndGet(); () }
            )

          // Write CAS first, then key-link, so readers never see dangling links.
          writeCas.flatMap {
            case false =>
              moduleIOMonad.unit
            case true  =>
              val keyHash = keyHashValue(key)
              val linkPath = keyPath(keyHash)
              onError(
                platformIO.writeDoc(linkPath, Doc.text(outputHash.toIdent)).map {
                  _ =>
                    statsUpdate { putLinkWrites.incrementAndGet(); () }
                },
                (),
                _ => statsUpdate { putLinkWriteErrors.incrementAndGet(); () }
              )
          }
      }

    override def statsSnapshot: Option[String] =
      if (!statsEnabled) None
      else {
        val keyGenCallsV = keyGenCalls.get()
        val keyGenFailuresV = keyGenFailures.get()
        val interfaceMemoHitsV = interfaceMemoHits.get()
        val interfaceMemoMissesV = interfaceMemoMisses.get()
        val dependencyHashCallsV = dependencyHashCalls.get()
        val getCallsV = getCalls.get()
        val getHitsV = getHits.get()
        val getMissesV = getMisses.get()
        val getLinkReadErrorsV = getLinkReadErrors.get()
        val getLinkParseMissesV = getLinkParseMisses.get()
        val getCasReadErrorsV = getCasReadErrors.get()
        val getCasDecodeMissesV = getCasDecodeMisses.get()
        val putCallsV = putCalls.get()
        val putEncodeFailuresV = putEncodeFailures.get()
        val putCasAlreadyExistsV = putCasAlreadyExists.get()
        val putCasWritesV = putCasWrites.get()
        val putCasWriteErrorsV = putCasWriteErrors.get()
        val putLinkWritesV = putLinkWrites.get()
        val putLinkWriteErrorsV = putLinkWriteErrors.get()

        val getHitRate = ratioPct(getHitsV, getCallsV)
        val memoHitRate = ratioPct(interfaceMemoHitsV, interfaceMemoHitsV + interfaceMemoMissesV)
        Some(
          s"[compile-cache fs] cacheDir=$cacheDirLabel keyGenCalls=$keyGenCallsV keyGenFailures=$keyGenFailuresV " +
            s"memoHits=$interfaceMemoHitsV memoMisses=$interfaceMemoMissesV memoHitRate=$memoHitRate " +
            s"dependencyHashCalls=$dependencyHashCallsV " +
            s"getCalls=$getCallsV getHits=$getHitsV getMisses=$getMissesV getHitRate=$getHitRate " +
            s"getLinkReadErrors=$getLinkReadErrorsV getLinkParseMisses=$getLinkParseMissesV " +
            s"getCasReadErrors=$getCasReadErrorsV getCasDecodeMisses=$getCasDecodeMissesV " +
            s"""firstGetLinkReadError="${Option(firstGetLinkReadError.get()).getOrElse("")}" """ +
            s"""firstGetCasReadError="${Option(firstGetCasReadError.get()).getOrElse("")}" """ +
            s"putCalls=$putCallsV putEncodeFailures=$putEncodeFailuresV putCasAlreadyExists=$putCasAlreadyExistsV " +
            s"putCasWrites=$putCasWritesV putCasWriteErrors=$putCasWriteErrorsV putLinkWrites=$putLinkWritesV putLinkWriteErrors=$putLinkWriteErrorsV"
        )
      }
  }
}
