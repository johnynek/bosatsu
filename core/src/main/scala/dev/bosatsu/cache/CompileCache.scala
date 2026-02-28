package dev.bosatsu.cache

import cats.syntax.all._
import dev.bosatsu.hashing.{Algo, HashValue}
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
import org.typelevel.paiges.{Doc, Document}
import scala.collection.concurrent.TrieMap

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
  ): InferCache[F] { type Key = FsKey } =
    new FilesystemCache(cacheDir, platformIO)

  def sourceExprHash(pack: Package.Parsed): String = {
    val normalizedStatements = pack.program.collect {
      case _: Statement.PaddingStatement => None
      case _: Statement.Comment          => None
      case stmt                          => Some(stmt.replaceRegions(sentinelRegion))
    }.flatten
    val normalized = pack.copy(program = normalizedStatements)
    val serialized = Document[Package.Parsed].document(normalized).render(200)
    hashUtf8(serialized).hex
  }

  def interfaceHash(iface: Package.Interface): String =
    ProtoConverter
      .interfaceToProto(iface)
      .toOption
      .map(p => Algo.hashBytes[Algo.Blake3](p.toByteArray).hex)
      .getOrElse(hashUtf8(iface.toString).hex)

  def keyHashHex(key: FsKey): String =
    keyHashValue(key).hex

  def outputHashHex(pack: Package.Inferred): Option[String] =
    outputHashValue(pack).map(_.hex)

  private[cache] def keyHashValue(key: FsKey): HashValue[Algo.Blake3] = {
    val deps = key.depInterfaceHashes
      .map { case (pn, hash) => s"${pn.asString}=$hash" }
      .mkString("\n")
    val payload =
      s"""schema:${key.schemaVersion}
         |package:${key.packageName.asString}
         |mode:${key.compileOptions.mode}
         |optimize:${key.compileOptions.optimize}
         |compiler:${key.compilerIdentity}
         |phase:${key.phaseIdentity}
         |source:${key.sourceExprHash}
         |deps:
         |$deps
         |""".stripMargin
    hashUtf8(payload)
  }

  private[cache] def outputHashValue(
      pack: Package.Inferred
  ): Option[HashValue[Algo.Blake3]] =
    ProtoConverter
      .packageToProto(pack)
      .toOption
      .map(p => Algo.hashBytes[Algo.Blake3](p.toByteArray))

  private def hashUtf8(str: String): HashValue[Algo.Blake3] =
    Algo.hashBytes[Algo.Blake3](str.getBytes(utf8))

  private final class FilesystemCache[F[_], P](
      cacheDir: P,
      platformIO: PlatformIO[F, P]
  ) extends InferCache[F] {
    type Key = FsKey

    import platformIO.moduleIOMonad

    private val interfaceHashMemo = TrieMap.empty[Package.Interface, String]

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
      Algo.parseIdent.parseAll(str.trim).toOption.collect {
        case hashed if hashed.algo.name == blake3.name =>
          HashValue[Algo.Blake3](hashed.value.hex)
      }

    private def onError[A](fa: F[A], fallback: => A): F[A] =
      moduleIOMonad.handleError(fa)(_ => fallback)

    def generateKey(
        pack: Package.Parsed,
        depInterfaces: List[(PackageName, Package.Interface)],
        compileOptions: CompileOptions,
        compilerIdentity: String,
        phaseIdentity: String
    ): F[Key] =
      moduleIOMonad.pure {
        val depHashes = depInterfaces
          .map { case (name, iface) =>
            val hash =
              interfaceHashMemo.getOrElseUpdate(iface, interfaceHash(iface))
            (name, hash)
          }
          .sortBy(_._1)

        FsKey(
          packageName = pack.name,
          compileOptions = compileOptions,
          compilerIdentity = compilerIdentity,
          phaseIdentity = phaseIdentity,
          sourceExprHash = sourceExprHash(pack),
          depInterfaceHashes = depHashes,
          schemaVersion = schemaVersion
        )
      }

    def get(key: Key): F[Option[Package.Inferred]] = {
      val keyHash = keyHashValue(key)
      val linkPath = keyPath(keyHash)

      onError(
        platformIO.readUtf8(linkPath).map(parseHashIdent),
        None
      ).flatMap {
        case None             =>
          moduleIOMonad.pure(None)
        case Some(outputHash) =>
          val packagePath = casPath(outputHash)
          val read =
            platformIO.readPackages(packagePath :: Nil).map {
              case pack :: Nil if pack.name == key.packageName =>
                // Serialized package artifacts are tag-erased to Unit.
                Some(pack.asInstanceOf[Package.Inferred])
              case _ =>
                None
            }
          onError(read, None)
      }
    }

    def put(key: Key, value: Package.Inferred): F[Unit] =
      outputHashValue(value) match {
        case None             =>
          moduleIOMonad.unit
        case Some(outputHash) =>
          val packagePath = casPath(outputHash)

          val writeCas: F[Boolean] =
            onError(
              platformIO.fileExists(packagePath).flatMap {
                case true  =>
                  moduleIOMonad.pure(true)
                case false =>
                  platformIO.writePackages(value :: Nil, packagePath).as(true)
              },
              false
            )

          // Write CAS first, then key-link, so readers never see dangling links.
          writeCas.flatMap {
            case false =>
              moduleIOMonad.unit
            case true  =>
              val keyHash = keyHashValue(key)
              val linkPath = keyPath(keyHash)
              onError(
                platformIO.writeDoc(linkPath, Doc.text(outputHash.toIdent)),
                ()
              )
          }
      }
  }
}
