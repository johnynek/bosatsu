package org.bykn.bosatsu.truffle

class TruffleTest extends munit.FunSuite {
  test("add 12+32") {
    val exprNode = AdditionNodeGen.create(
        new IntNode(12),
        new IntNode(34));
    val rootNode = new BosatsuRootNode(exprNode)
    val callTarget = rootNode.getCallTarget()

    val result = callTarget.call()

    assertEquals(Integer.valueOf(46): AnyRef, result);
  }
}