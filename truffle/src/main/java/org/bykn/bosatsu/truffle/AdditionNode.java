package org.bykn.bosatsu.truffle;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;

@NodeChild("leftNode") @NodeChild("rightNode")
public abstract class AdditionNode extends BosatsuNode {
    @Specialization
    protected int addInts(int leftValue, int rightValue) {
      return leftValue + rightValue;
    }
}
