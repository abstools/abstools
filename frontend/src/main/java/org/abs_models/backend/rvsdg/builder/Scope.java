package org.abs_models.backend.rvsdg.builder;

import org.abs_models.backend.rvsdg.abs.Variable;
import org.abs_models.frontend.typechecker.Type;

import java.util.HashMap;

public class Scope {
    final Function function;
    final Scope parent;
    final HashMap<String, Variable> vars = new HashMap<>();

    private Scope(Function function, Scope parent) {
        this.function = function;
        this.parent = parent;
    }

    public Scope(Function function) {
        this.function = function;
        this.parent = null;
    }

    public Scope createChild() {
        return new Scope(this.function, this);
    }

    public Variable declareVar(String name, Type type) {
        Variable var = new Variable(name, type);
        vars.put(name, var);
        this.function.declareVar(var);
        return var;
    }

    public Variable lookupVar(String name) {
        Variable var = vars.get(name);
        if (var == null) {
            if (this.parent == null) {
                throw new RuntimeException("Variable not decalred: " + name);
            }

            var = this.parent.lookupVar(name);
            this.vars.put(name, var);
        }
        return var;
    }
}
