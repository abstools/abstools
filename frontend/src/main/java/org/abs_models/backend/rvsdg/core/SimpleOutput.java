package org.abs_models.backend.rvsdg.core;

import org.abs_models.frontend.typechecker.Type;

/**
 * The output from a node.
 */
public class SimpleOutput extends Output {
    public final Node node;
    public final int idx;

    public SimpleOutput(Type type,  Node node, int idx) {
        super(type);
        this.node = node;
        this.idx = idx;
    }
}
