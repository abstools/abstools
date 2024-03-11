package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Input;
import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;
import org.abs_models.frontend.typechecker.Type;

public class ToRationalNode extends Node {
    public ToRationalNode(Region region, Output num, Output den, Type type) {
        super(region);

        this.addSimpleInput(num);
        this.addSimpleInput(den);
        this.addSimpleOutput(type);
    }

    public Input getNumerator() {
        return inputs.get(0);
    }

    public Input getDenominator() {
        return inputs.get(1);
    }

    public Output getResult() {
        return outputs.get(0);
    }
}
