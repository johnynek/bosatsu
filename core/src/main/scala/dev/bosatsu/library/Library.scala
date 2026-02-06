package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}

object Library {
  def defaultFileName(name: Name, version: Version): String =
    s"${name.name}-v${version.render}.bosatsu_lib"

  def dep(name: String, version: Option[Version]): proto.LibDependency =
    proto.LibDependency(
      name = name,
      desc = version.map(ver => proto.LibDescriptor(version = Some(ver.toProto)))
    )

  def dep(name: String, version: Version): proto.LibDependency =
    dep(name, Some(version))

  def dep(name: Name, version: Version): proto.LibDependency =
    dep(name.name, version)

  def dep(name: String, desc: proto.LibDescriptor): proto.LibDependency =
    proto.LibDependency(name = name, desc = Some(desc))

  def dep(name: Name, desc: proto.LibDescriptor): proto.LibDependency =
    dep(name.name, desc)

  def getVersion(dep: proto.LibDependency): Option[Version] =
    dep.desc.flatMap(_.version).map(Version.fromProto(_))

  def getVersion(lib: proto.Library): Option[Version] =
    lib.descriptor.flatMap(_.version).map(Version.fromProto(_))

  def versionOrZero(dep: proto.LibDependency): Version =
    getVersion(dep).getOrElse(Version.zero)
}
