package dev.bosatsu

/** The identifiers in C source, which the C code generator's runtime names are
  * checked against when the compiler is compiled (see
  * [[CheckedNames.cRuntime]]).
  */
object CRuntimeNames {
  private def isIdentStart(c: Char): Boolean =
    ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')) || (c == '_')

  private def isIdentPart(c: Char): Boolean =
    isIdentStart(c) || ((c >= '0') && (c <= '9'))

  /** The identifiers in `text`, outside comments and string and character
    * literals. A token that starts with a digit is a number, so its letters
    * (as in `0x1fULL`) are not identifiers.
    */
  def identifiers(text: String): Set[String] = {
    val result = Set.newBuilder[String]
    val len = text.length
    var idx = 0
    while (idx < len) {
      val c = text.charAt(idx)
      val next = if (idx + 1 < len) text.charAt(idx + 1) else ' '
      if ((c == '/') && (next == '/')) {
        val end = text.indexOf('\n', idx)
        idx = if (end < 0) len else end
      } else if ((c == '/') && (next == '*')) {
        val end = text.indexOf("*/", idx + 2)
        idx = if (end < 0) len else end + 2
      } else if ((c == '"') || (c == '\'')) {
        idx += 1
        while ((idx < len) && (text.charAt(idx) != c)) {
          // skip the escaped character too
          idx += (if (text.charAt(idx) == '\\') 2 else 1)
        }
        idx += 1
      } else if (isIdentPart(c)) {
        val start = idx
        while ((idx < len) && isIdentPart(text.charAt(idx))) idx += 1
        if (isIdentStart(c)) result += text.substring(start, idx)
      } else idx += 1
    }
    result.result()
  }
}
