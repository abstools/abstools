package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Input;
import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;
import org.abs_models.frontend.typechecker.Type;

public class ComparisonNode extends Node {
    public enum Operator {
        Eq,
        NotEq,
        Lt,
        Lte,
        Gt,
        Gte,
    }

    public final Operator operator;

    public ComparisonNode(Region region, Operator operator, Output left, Output right, Type boolType) {
        super(region);

        this.operator = operator;
        this.addSimpleInput(left);
        this.addSimpleInput(right);
        this.addSimpleOutput(boolType);
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
