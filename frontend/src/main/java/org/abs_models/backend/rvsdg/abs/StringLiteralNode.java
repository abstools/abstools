package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;
import org.abs_models.frontend.typechecker.Type;

public class StringLiteralNode extends Node {
    public final String content;

    public StringLiteralNode(Region region, Type type, String content) {
        super(region);
        this.content = content;

        addSimpleOutput(type);
    }

    public Output getResult() {
        return outputs.get(0);
    }
}
