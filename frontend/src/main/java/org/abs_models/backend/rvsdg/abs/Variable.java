package org.abs_models.backend.rvsdg.abs;

import org.abs_models.frontend.typechecker.Type;

public class Variable {
    final String name;
    final Type type;

    public Variable(String name, Type type) {
        this.name = name;
        this.type = type;
    }
}
