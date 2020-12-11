package org.abs_models.backend.rvsdg.core;

import org.abs_models.frontend.typechecker.Type;

/**
 * An input that is connected to the output from another node.
 */
public class SimpleInput extends Input {
    final public Output output;

    public SimpleInput(Output output) {
        this.output = output;
    }

    @Override
    Type getType() {
        return this.output.type;
    }
}
