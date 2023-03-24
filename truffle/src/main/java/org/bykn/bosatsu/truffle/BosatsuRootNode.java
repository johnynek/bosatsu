package org.bykn.bosatsu.truffle;

import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.frame.VirtualFrame;

class BosatsuRootNode extends RootNode {
    @SuppressWarnings("FieldMayBeFinal")
    @Child
    private BosatsuNode exprNode;

    public BosatsuRootNode(BosatsuNode exprNode) {
        super(null);

        this.exprNode = exprNode;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return this.exprNode.executeInt(frame);
    }
}
