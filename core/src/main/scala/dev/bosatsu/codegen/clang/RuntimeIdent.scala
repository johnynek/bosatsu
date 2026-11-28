package dev.bosatsu.codegen.clang

/** A C runtime function or macro that the generated C calls. The C code
  * generator names each one through this, as a string literal, so that
  * `ReservedNamesTest` can find them and check that each occurs in one of the
  * c_runtime headers: a misspelled name fails that test rather than producing
  * C that fails to compile. It is a test, not a compile-time check, so that
  * editing a header doesn't recompile core.
  */
object RuntimeIdent {
  def apply(name: String): Code.Ident = Code.Ident(name)
}
