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
  * registered external, against what the source declares.
  *
  * The test finds those names by scanning the compiler's sources, so use only
  * the shapes it reads, and it fails on any other:
  *   - `ReservedPackage.Prog.external("pure")`, or the same through an
  *     unrenamed or renamed import of `Prog` from `ReservedPackage`, possibly
  *     split across lines before the `.external`;
  *   - `.name` or `.source` on one of these packages.
  *
  * Each argument must be a string literal. Don't import `ReservedPackage.*`,
  * rename `ReservedPackage` itself, or pass a package around as a value.
  */
object ReservedPackage {
  enum Reserved(val source: String, packageName: String) {
    case Prog extends Reserved("test_workspace/Prog.bosatsu", "Bosatsu/Prog")
    case Int64
        extends Reserved("test_workspace/Int64.bosatsu", "Bosatsu/Num/Int64")
    case Float64
        extends Reserved("test_workspace/Float64.bosatsu", "Bosatsu/Num/Float64")
    case Lazy
        extends Reserved("test_workspace/Bosatsu/Lazy.bosatsu", "Bosatsu/Lazy")
    case Eval
        extends Reserved("test_workspace/Bosatsu/Eval.bosatsu", "Bosatsu/Eval")
    case IoCore
        extends Reserved(
          "test_workspace/Bosatsu/IO/Core.bosatsu",
          "Bosatsu/IO/Core"
        )
    case IoBytes
        extends Reserved(
          "test_workspace/Bosatsu/IO/Bytes.bosatsu",
          "Bosatsu/IO/Bytes"
        )
    case Array
        extends Reserved(
          "test_workspace/Bosatsu/Collection/Array.bosatsu",
          "Bosatsu/Collection/Array"
        )
    case Json
        extends Reserved("test_workspace/Bosatsu/Json.bosatsu", "Bosatsu/Json")

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

  export Reserved.{Prog, Int64, Float64, Lazy, Eval, IoCore, IoBytes, Array, Json}

  /** Every reserved package above */
  val all: List[Reserved] = Reserved.values.toList
}
