package org.bykn.bosatsu.truffle;
import com.oracle.truffle.api.dsl.NodeChild;

  @NodeChild("leftNode")  @NodeChild("rightNode")
public abstract class AdditionNode extends BosatsuNode {
    protected int addInts(int leftValue, int rightValue) {
        return leftValue + rightValue;
    }
}
