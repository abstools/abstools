package org.abs_models.backend.rvsdg.core;

import org.abs_models.frontend.typechecker.Type;

import java.util.ArrayList;
import java.util.List;

public class Node {
    final public List<Input> inputs = new ArrayList<>();
    final public List<Output> outputs = new ArrayList<>();
    final public Region region;

    public Node(Region region) {
        this.region = region;
        region.addNode(this);
    }

    public SimpleInput addSimpleInput(Output value) {
        SimpleInput input = new SimpleInput(value);
        inputs.add(input);
        return input;
    }

    public SimpleOutput addSimpleOutput(Type type) {
        SimpleOutput output = new SimpleOutput(type, this, outputs.size());
        outputs.add(output);
        return output;
    }
}
