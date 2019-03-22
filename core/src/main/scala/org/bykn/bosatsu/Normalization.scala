package org.bykn.bosatsu

object Normalization {
  sealed abstract class NormalExpression {
    val maxLambdaVar: Option[Int]
  }

  case class NormalNothing() extends NormalExpression {
    val maxLambdaVar = None
  }

  case class NormalExpressionTag(ne: NormalExpression, children: List[NormalExpression])
}

