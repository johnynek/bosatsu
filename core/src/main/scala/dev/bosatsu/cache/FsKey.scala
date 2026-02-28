package dev.bosatsu.cache

import dev.bosatsu.{CompileOptions, PackageName}

final case class FsKey(
    packageName: PackageName,
    compileOptions: CompileOptions,
    compilerIdentity: String,
    phaseIdentity: String,
    sourceExprHash: String,
    depInterfaceHashes: List[(PackageName, String)],
    schemaVersion: Int
)
