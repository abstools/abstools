package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Input;
import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;
import org.abs_models.frontend.typechecker.Type;

public class IntToRationalNode extends Node {
    public IntToRationalNode(Region region, Output num, Type type) {
        super(region);

        this.addSimpleInput(num);
        this.addSimpleOutput(type);
    }

    public Input getInt() {
        return inputs.get(0);
    }

    public Output getResult() {
        return outputs.get(0);
    }
}
