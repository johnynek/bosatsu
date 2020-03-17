package org.bykn.bosatsu

import Normalization.NormalExpressionTag

object NormalEvaluation {

}

case class NormalEvaluation(packs: PackageMap.Typed[(Declaration, NormalExpressionTag)], externals: Externals) {
  def evaluateLast(p: PackageName): Option[NormalExpression] = packs.toMap(p).program.lets.lastOption.map(_._3.tag._2.ne)
  def evaluate(p: PackageName, name: Identifier): Option[NormalExpression] = packs.toMap(p).program.lets.lastOption.map(_._3.tag._2.ne)
}
