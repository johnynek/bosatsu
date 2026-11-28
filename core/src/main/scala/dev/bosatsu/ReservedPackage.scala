package dev.bosatsu

import dev.bosatsu.Identifier.{Bindable, Constructor, Name}

/** The reserved packages besides Predef that the compiler names, one
  * definition each. Their sources are in test_workspace.
  *
  * Unlike Predef's names (see [[PredefName]]), these are not checked when the
  * compiler is compiled: reading these sources then would make every edit to
  * them recompile all of core. `ReservedNamesTest` checks them instead. It
  * checks each package name against its source's `package` line. It also
  * checks each name passed to `external`, `value` or `typeName`, and each
  * registered external, against what the source declares. The test finds those
  * names by scanning the compiler's sources, so pass each one as a string
  * literal.
  */
object ReservedPackage {
  sealed abstract class Reserved(val source: String, packageName: String) {
    val name: PackageName = Parser.unsafeParse(PackageName.parser, packageName)

    /** An external of this package, as the key [[Externals]] registers its
      * implementation under
      */
    def external(n: String): (PackageName, Bindable) = (name, Name(n))

    /** A value in this package's export list */
    def value(n: String): Name = Name(n)

    /** A type in this package's export list */
    def typeName(n: String): TypeName = TypeName(Constructor(n))
  }

  object Prog extends Reserved("test_workspace/Prog.bosatsu", "Bosatsu/Prog")

  object Int64
      extends Reserved("test_workspace/Int64.bosatsu", "Bosatsu/Num/Int64")

  object Float64
      extends Reserved("test_workspace/Float64.bosatsu", "Bosatsu/Num/Float64")

  object Lazy
      extends Reserved("test_workspace/Bosatsu/Lazy.bosatsu", "Bosatsu/Lazy")

  object Eval
      extends Reserved("test_workspace/Bosatsu/Eval.bosatsu", "Bosatsu/Eval")

  object IoCore
      extends Reserved("test_workspace/Bosatsu/IO/Core.bosatsu", "Bosatsu/IO/Core")

  object IoBytes
      extends Reserved(
        "test_workspace/Bosatsu/IO/Bytes.bosatsu",
        "Bosatsu/IO/Bytes"
      )

  object Array
      extends Reserved(
        "test_workspace/Bosatsu/Collection/Array.bosatsu",
        "Bosatsu/Collection/Array"
      )

  object Json
      extends Reserved("test_workspace/Bosatsu/Json.bosatsu", "Bosatsu/Json")

  /** Every reserved package above */
  val all: List[Reserved] =
    List(Prog, Int64, Float64, Lazy, Eval, IoCore, IoBytes, Array, Json)
}
