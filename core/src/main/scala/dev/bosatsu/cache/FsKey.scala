package dev.bosatsu.cache

import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.{CompileOptions, Package, PackageName}
import scala.collection.immutable.SortedMap

/** Filesystem cache key plus decode context.
  *
  * We keep both dependency hashes and dependency interfaces on purpose:
  * - `depInterfaceHashes` are part of cache identity and are what `keyHashValue`
  *   hashes for stable key lookup.
  * - `depInterfaces` are required when reading cached package bytes back, because
  *   package decoding needs dependency interfaces in scope.
  *
  * Other fields are also part of identity:
  * - package/mode/options/compiler/phase/schema ensure we do not mix artifacts
  *   across incompatible compiler runs.
  * - `sourceHash` invalidates cache entries when source identity changes.
  */
final case class FsKey(
    packageName: PackageName,
    compileOptions: CompileOptions,
    compilerIdentity: String,
    phaseIdentity: String,
    sourceHash: HashValue[Algo.Blake3],
    depInterfaceHashes: SortedMap[PackageName, HashValue[Algo.Blake3]],
    depInterfaces: SortedMap[PackageName, Package.Interface],
    schemaVersion: Int
)
