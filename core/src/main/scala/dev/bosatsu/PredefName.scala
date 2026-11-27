package dev.bosatsu

import dev.bosatsu.Identifier.{Bindable, Constructor, Name}

/** Predef names that the compiler spells out, checked against the Predef
  * source when the compiler is compiled: a misspelled name fails to compile
  * rather than failing when a program reaches it. Each argument must be a
  * string literal. See [[CheckedNames]].
  */
object PredefName {

  /** The Predef source, which [[Predef.predefString]] embeds */
  inline val source = "core/src/main/resources/bosatsu/predef.bosatsu"

  /** A value in Predef's export list */
  inline def value(inline name: String): Name =
    Name(CheckedNames.exportedValue(source, name))

  /** A constructor of a type that Predef exports with its constructors */
  inline def constructor(inline name: String): Constructor =
    Constructor(CheckedNames.constructor(source, name))

  /** A type in Predef's export list */
  inline def typeName(inline name: String): TypeName =
    TypeName(Constructor(CheckedNames.exportedType(source, name)))

  /** An external that Predef declares, as the key [[Externals]] registers its
    * implementation under
    */
  inline def external(inline name: String): (PackageName, Bindable) =
    (PackageName.PredefName, Name(CheckedNames.external(source, name)))
}

/** The Predef names that the compiler spells out in more than one place */
object PredefNames {
  val True: Constructor = PredefName.constructor("True")
  val False: Constructor = PredefName.constructor("False")
  val EmptyList: Constructor = PredefName.constructor("EmptyList")
  val NonEmptyList: Constructor = PredefName.constructor("NonEmptyList")
  val Unit: Constructor = PredefName.constructor("Unit")
  val Some: Constructor = PredefName.constructor("Some")
  val LT: Constructor = PredefName.constructor("LT")
  val EQ: Constructor = PredefName.constructor("EQ")
  val GT: Constructor = PredefName.constructor("GT")

  val add: Name = PredefName.value("add")
  val sub: Name = PredefName.value("sub")
  val mul: Name = PredefName.value("mul")
  val cmp_Int: Name = PredefName.value("cmp_Int")
  val eq_Int: Name = PredefName.value("eq_Int")
  val char_to_String: Name = PredefName.value("char_to_String")
  val concat_String: Name = PredefName.value("concat_String")
}
