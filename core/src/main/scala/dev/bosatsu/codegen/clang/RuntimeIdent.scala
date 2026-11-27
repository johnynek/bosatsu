package dev.bosatsu.codegen.clang

import dev.bosatsu.CheckedNames

/** A C runtime function or macro that the generated C calls, checked when the
  * compiler is compiled to occur in one of the c_runtime headers: a misspelled
  * name fails to compile rather than producing C that fails to compile. The
  * argument must be a string literal. See [[CheckedNames.cRuntime]].
  */
object RuntimeIdent {
  inline def apply(inline name: String): Code.Ident =
    Code.Ident(CheckedNames.cRuntime(name))
}
