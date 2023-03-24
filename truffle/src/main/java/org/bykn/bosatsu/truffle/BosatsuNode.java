package org.bykn.bosatsu.truffle;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;

public abstract class BosatsuNode extends Node {
  public abstract int executeInt(VirtualFrame frame);
}
