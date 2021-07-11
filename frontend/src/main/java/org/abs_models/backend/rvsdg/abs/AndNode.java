package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Input;
import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;

public class AndNode extends Node {
    public AndNode(Region region, Output left, Output right) {
        super(region);

        this.addSimpleInput(left);
        this.addSimpleInput(right);
        this.addSimpleOutput(left.type);
    }

    public Input getLeft() {
        return inputs.get(0);
    }

    public Input getRight() {
        return inputs.get(1);
    }

    public Output getResult() {
        return outputs.get(0);
    }
}
