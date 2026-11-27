package dev.bosatsu

import dev.bosatsu.Identifier.{Bindable, Constructor, Name}

/** The reserved packages besides Predef that the compiler names, one
  * definition each. Their sources are in test_workspace, and each package name,
  * and each of its names that the compiler spells out, is checked against that
  * source when the compiler is compiled (see [[CheckedNames]]). Each argument
  * must be a string literal.
  */
object ReservedPackage {
  private inline def checkedName(
      inline source: String,
      inline name: String
  ): PackageName =
    Parser.unsafeParse(
      PackageName.parser,
      CheckedNames.packageName(source, name)
    )

  object Prog {
    inline val source = "test_workspace/Prog.bosatsu"
    val name: PackageName = checkedName(source, "Bosatsu/Prog")

    /** An external of this package, as the key [[Externals]] registers its
      * implementation under
      */
    inline def external(inline n: String): (PackageName, Bindable) =
      (name, Name(CheckedNames.external(source, n)))

    inline def typeName(inline n: String): TypeName =
      TypeName(Constructor(CheckedNames.exportedType(source, n)))
  }

  object Int64 {
    inline val source = "test_workspace/Int64.bosatsu"
    val name: PackageName = checkedName(source, "Bosatsu/Num/Int64")

    inline def external(inline n: String): (PackageName, Bindable) =
      (name, Name(CheckedNames.external(source, n)))

    inline def value(inline n: String): Name =
      Name(CheckedNames.exportedValue(source, n))

    inline def typeName(inline n: String): TypeName =
      TypeName(Constructor(CheckedNames.exportedType(source, n)))
  }

  object Float64 {
    inline val source = "test_workspace/Float64.bosatsu"
    val name: PackageName = checkedName(source, "Bosatsu/Num/Float64")

    inline def external(inline n: String): (PackageName, Bindable) =
      (name, Name(CheckedNames.external(source, n)))

    inline def value(inline n: String): Name =
      Name(CheckedNames.exportedValue(source, n))
  }

  object Lazy {
    inline val source = "test_workspace/Bosatsu/Lazy.bosatsu"
    val name: PackageName = checkedName(source, "Bosatsu/Lazy")

    inline def external(inline n: String): (PackageName, Bindable) =
      (name, Name(CheckedNames.external(source, n)))

    inline def value(inline n: String): Name =
      Name(CheckedNames.exportedValue(source, n))

    inline def typeName(inline n: String): TypeName =
      TypeName(Constructor(CheckedNames.exportedType(source, n)))
  }

  object Eval {
    inline val source = "test_workspace/Bosatsu/Eval.bosatsu"
    val name: PackageName = checkedName(source, "Bosatsu/Eval")

    inline def external(inline n: String): (PackageName, Bindable) =
      (name, Name(CheckedNames.external(source, n)))

    inline def value(inline n: String): Name =
      Name(CheckedNames.exportedValue(source, n))

    inline def typeName(inline n: String): TypeName =
      TypeName(Constructor(CheckedNames.exportedType(source, n)))
  }

  object IoCore {
    inline val source = "test_workspace/Bosatsu/IO/Core.bosatsu"
    val name: PackageName = checkedName(source, "Bosatsu/IO/Core")

    inline def external(inline n: String): (PackageName, Bindable) =
      (name, Name(CheckedNames.external(source, n)))
  }

  object IoBytes {
    inline val source = "test_workspace/Bosatsu/IO/Bytes.bosatsu"
    val name: PackageName = checkedName(source, "Bosatsu/IO/Bytes")

    inline def external(inline n: String): (PackageName, Bindable) =
      (name, Name(CheckedNames.external(source, n)))

    inline def typeName(inline n: String): TypeName =
      TypeName(Constructor(CheckedNames.exportedType(source, n)))
  }

  object Array {
    inline val source = "test_workspace/Bosatsu/Collection/Array.bosatsu"
    val name: PackageName = checkedName(source, "Bosatsu/Collection/Array")

    inline def external(inline n: String): (PackageName, Bindable) =
      (name, Name(CheckedNames.external(source, n)))

    inline def value(inline n: String): Name =
      Name(CheckedNames.exportedValue(source, n))

    inline def typeName(inline n: String): TypeName =
      TypeName(Constructor(CheckedNames.exportedType(source, n)))
  }

  object Json {
    inline val source = "test_workspace/Bosatsu/Json.bosatsu"
    val name: PackageName = checkedName(source, "Bosatsu/Json")

    inline def typeName(inline n: String): TypeName =
      TypeName(Constructor(CheckedNames.exportedType(source, n)))
  }
}
