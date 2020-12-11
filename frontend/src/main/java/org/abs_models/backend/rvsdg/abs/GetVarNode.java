package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.*;

public class GetVarNode extends Node {
    final Variable var;

    public GetVarNode(Region region, Output state, Variable var) {
        super(region);
        this.var = var;

        inputs.add(new SimpleInput(state));
        addSimpleInput(state);
        addSimpleOutput(state.type);
        addSimpleOutput(var.type);
    }

    Output getResult() {
        return outputs.get(1);
    }

    Output getStateOutput() {
        return outputs.get(0);
    }
}
