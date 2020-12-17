package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.*;

public class GetVarNode extends Node {
    public final Variable var;

    public GetVarNode(Region region, Output state, Variable var) {
        super(region);
        this.var = var;

        addSimpleInput(state);
        addSimpleOutput(state.type);
        addSimpleOutput(var.type);
    }

    public Input getState() {
        return inputs.get(0);
    }

    public Output getResult() {
        return outputs.get(1);
    }

    public Output getNewState() {
        return outputs.get(0);
    }
}
