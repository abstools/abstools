package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;
import org.abs_models.backend.rvsdg.core.SimpleInput;

public class SetVarNode extends Node {
    final Variable var;

    public SetVarNode(Region region, Output oldState, Variable var, Output newValue) {
        super(region);
        this.var = var;

        addSimpleInput(oldState);
        addSimpleInput(newValue);
        addSimpleOutput(oldState.type);
    }

    public Output getNewState() {
        return outputs.get(0);
    }
}
