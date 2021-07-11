package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Input;
import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;
import org.abs_models.frontend.typechecker.Type;

public class ToStringNode extends Node {
    public ToStringNode(Region region, Type type, Output value) {
        super(region);

        this.addSimpleInput(value);
        this.addSimpleOutput(type);
    }

    public Input getValue() {
        return inputs.get(0);
    }

    public Output getResult() {
        return outputs.get(0);
    }
}
