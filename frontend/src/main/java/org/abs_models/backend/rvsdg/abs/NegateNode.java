package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Input;
import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;
import org.abs_models.frontend.typechecker.Type;

public class NegateNode extends Node {
    public NegateNode(Region region, Output base, Type type) {
        super(region);

        this.addSimpleInput(base);
        this.addSimpleOutput(type);
    }

    public Input getBase() {
        return inputs.get(0);
    }

    public Output getResult() {
        return outputs.get(0);
    }
}
