package org.bykn.bosatsu

import org.bykn.bosatsu.rankn._

import fastparse.all.Parsed

object TestUtils {
  import TestParseUtils.region

  def typeEnvOf(pack: PackageName, str: String): TypeEnv[Unit] = {

    val tpeFn: String => Type.Const =
      { tpe => Type.Const.Defined(pack, tpe) }

    val consFn: String => (PackageName, ConstructorName) =
      { cons => (pack, ConstructorName(cons)) }

    Statement.parser.parse(str) match {
      case Parsed.Success(stmt, idx) =>
        assert(idx == str.length)
        val prog = Program.fromStatement(
          Predef.packageName,
          tpeFn,
          consFn,
          stmt)
        prog.types
      case Parsed.Failure(exp, idx, extra) =>
        sys.error(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
    }
  }
}
