package org.abs_models.backend.rvsdg.core;

import org.abs_models.frontend.typechecker.Type;

abstract public class Output {
    final public Type type;

    public Output(Type type) {
        this.type = type;
    }
}
