package org.bykn.bosatsu

import org.bykn.bosatsu.rankn._
import cats.data.Validated
import org.scalatest.{Assertion, Assertions}

import fastparse.all.Parsed

import Assertions.fail

object TestUtils {
  import TestParseUtils.region

  def typeEnvOf(pack: PackageName, str: String): TypeEnv[Unit] = {

    val tpeFn: Identifier.Constructor => Type.Const =
      { tpe => Type.Const.Defined(pack, TypeName(tpe)) }

    val consFn: Identifier.Constructor => (PackageName, Identifier.Constructor) =
      { cons => (pack, cons) }

    val stmt = statementOf(pack, str)
    val prog = Program.fromStatement(
      Predef.packageName,
      tpeFn,
      consFn,
      stmt)
    TypeEnv.fromParsed(prog.types)
  }

  def statementOf(pack: PackageName, str: String): Statement = {

    val tpeFn: Identifier.Constructor => Type.Const =
      { tpe => Type.Const.Defined(pack, TypeName(tpe)) }

    val consFn: Identifier.Constructor => (PackageName, Identifier.Constructor) =
      { cons => (pack, cons) }

    Statement.parser.parse(str) match {
      case Parsed.Success(stmt, idx) =>
        assert(idx == str.length)
        stmt
      case Parsed.Failure(exp, idx, extra) =>
        sys.error(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
    }
  }

  def checkLast(statement: String)(fn: TypedExpr[Declaration] => Assertion): Assertion =
    Statement.parser.parse(statement) match {
      case Parsed.Success(stmt, _) =>
        Package.inferBody(PackageName.parts("Test"), Nil, stmt) match {
          case Validated.Invalid(errs) => fail(errs.toList.map(_.message(Map.empty)).mkString("\n"))
          case Validated.Valid((_, lets)) =>
            fn(lets.last._3)
        }
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $statement: $exp at $idx with trace: ${extra.traced.trace}")
    }
}
