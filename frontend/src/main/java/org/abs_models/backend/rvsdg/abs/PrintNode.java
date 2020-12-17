package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Input;
import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;

public class PrintNode extends Node {
    public final boolean withNewline;

    public PrintNode(Region region, Output state, Output value, boolean withNewline) {
        super(region);

        this.withNewline = withNewline;

        addSimpleInput(state);
        addSimpleInput(value);
        addSimpleOutput(state.type);
    }

    public Input getState() {
        return inputs.get(0);
    }

    public Input getValue() {
        return inputs.get(1);
    }

    public Output getNewState() {
        return outputs.get(0);
    }
}
