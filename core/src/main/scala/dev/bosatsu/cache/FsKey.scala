package dev.bosatsu.cache

import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.{CompileOptions, PackageName}
import scala.collection.immutable.SortedMap

final case class FsKey(
    packageName: PackageName,
    compileOptions: CompileOptions,
    compilerIdentity: String,
    phaseIdentity: String,
    sourceExprHash: HashValue[Algo.Blake3],
    depInterfaceHashes: SortedMap[PackageName, HashValue[Algo.Blake3]],
    schemaVersion: Int
)
