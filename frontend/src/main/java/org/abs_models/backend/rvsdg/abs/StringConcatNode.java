package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Input;
import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;

public class StringConcatNode extends Node {
    public StringConcatNode(Region region, Output left, Output right) {
        super(region);

        addSimpleInput(left);
        addSimpleInput(right);
        addSimpleOutput(left.type);
    }

    public Output getResult() {
        return outputs.get(0);
    }

    public Input getLeft() {
        return inputs.get(0);
    }

    public Input getRight() {
        return inputs.get(1);
    }
}
