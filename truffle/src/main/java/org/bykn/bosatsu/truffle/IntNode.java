package org.bykn.bosatsu.truffle;

import com.oracle.truffle.api.frame.VirtualFrame;

public class IntNode extends BosatsuNode {
  private final int value;

  public IntNode(int value) {
    this.value = value;
  }

  @Override
  public int executeInt(VirtualFrame frame) {
    return this.value;
  }
}
