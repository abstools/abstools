package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;
import org.abs_models.frontend.typechecker.Type;

public class FloatLiteralNode extends Node {
    public final double content;

    public FloatLiteralNode(Region region, Type type, double content) {
        super(region);
        this.content = content;

        this.addSimpleOutput(type);
    }

    public Output getResult() {
        return outputs.get(0);
    }
}
