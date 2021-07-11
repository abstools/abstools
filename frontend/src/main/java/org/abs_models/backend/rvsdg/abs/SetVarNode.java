package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.*;

public class SetVarNode extends Node {
    final public Variable var;

    public SetVarNode(Region region, Output oldState, Variable var, Output newValue) {
        super(region);
        this.var = var;

        addSimpleInput(oldState);
        addSimpleInput(newValue);
        addSimpleOutput(oldState.type);
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
