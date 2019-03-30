package org.bykn.bosatsu

import fastparse.all._

object Operators {

  // Longer strings bind tighter, so ** binds tighter than *
  def compareOperator(left: String, right: String): Int = {
    def loop(idx: Int): Int = {
      val leftDone = left.length <= idx
      val rightDone = right.length <= idx
      (leftDone, rightDone) match {
        case (true, true) => 0
        case (true, false) => 1
        case (false, true) => -1
        case (false, false) =>
          val lc = left.substring(idx, idx + 1)
          val rc = right.substring(idx, idx + 1)
          if (lc == rc) loop(idx + 1)
          else {
            Integer.compare(
              priorityMap.getOrElse(lc, Int.MaxValue),
              priorityMap.getOrElse(rc, Int.MaxValue))
          }
      }
    }

    if (left eq right) 0
    else loop(0)
  }

  private val singleToks =
    List(
      "*", "/", "%",
      "+", "-",
      "<", ">",
      "!", "$",
      "&", "|", "^",
      "?", "~").map(_.intern)

  private val priorityMap: Map[String, Int] =
    (singleToks ::: List("="))
      .iterator
      .zipWithIndex
      .toMap

  /**
   * Here are a list of operators we allow
   */
  val operatorToken: P[String] = {
    def from(strs: Iterable[String]): P[Unit] =
      strs.map(P(_)).reduce(_ | _)

    val singles = from(singleToks)
    // = can appear with at least one other character
    val withEqual = from("=" :: singleToks).rep(min = 2)
    // we can also repeat core operators one or more times
    val noEqual = singles.rep(min = 1)
    (withEqual | noEqual).!.map(_.intern)
  }

  sealed abstract class Formula[+A] {
    override def toString: String =
      this match {
        case Formula.Sym(a) => a.toString
        case Formula.Op(a, op, b) =>
          s"($a $op $b)"
      }
  }

  object Formula {
    case class Sym[A](value: A) extends Formula[A]
    case class Op[A](left: Formula[A], op: String, right: Formula[A]) extends Formula[A]

    /**
     * 1 * 2 + 3 => (1 * 2) + 3
     * 1 * 2 * 3 => ((1 * 2) * 3)
     */
    def toFormula[A](init: Formula[A], rest: List[(String, Formula[A])]): Formula[A] =
      rest match {
        case Nil => init
        case (op, next) :: Nil => Op(init, op, next)
        case (op1, next1) :: (right@((op2, next2) :: tail)) =>
          val c = compareOperator(op1, op2)
          if (c > 0) {
            // right binds tighter
            // 1 + 2 * 3 .... => 1 + toFormula(2 * 3 ...)
            val f2 = Op(next1, op2, next2)
            toFormula(init, (op1, f2) :: tail)
          }
          else {
            // 1 + 2 + 3 => (1 + 2) + 3
            // 1 * 2 + 3 => (1 * 2) + 3
            toFormula(Op(init, op1, next1), right)
          }
      }

    /**
     * Parse a chain of at least 1 operator being applied
     */
    def infixOps1[A](p: P[A]): P[A => Formula[A]] = {
      val chain: P[List[(String, A)]] =
        P(Parser.maybeSpace ~ operatorToken ~ Parser.maybeSpacesAndLines ~ p)
          .rep(min = 1)
          .map(_.toList)

      chain.map { rest =>

        { a: A => toFormula(Sym(a), rest.map { case (o, s) => (o, Sym(s)) }) }
      }
    }
    /**
     * An a formula is a series of A's separated by spaces, with
     * the correct parenthesis
     */
    def parser[A](p: P[A]): P[Formula[A]] =
      (p ~ (infixOps1(p)).?)
        .map {
          case (a, None) => Sym(a)
          case (a, Some(f)) => f(a)
        }
  }
}
