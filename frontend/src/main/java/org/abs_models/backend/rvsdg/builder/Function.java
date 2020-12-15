package org.abs_models.backend.rvsdg.builder;

import org.abs_models.backend.rvsdg.abs.StateOutput;
import org.abs_models.backend.rvsdg.abs.Variable;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;
import org.abs_models.frontend.ast.Block;
import org.abs_models.frontend.ast.Model;

import java.util.ArrayList;
import java.util.List;

public class Function {
    public final List<Variable> variables = new ArrayList<>();
    public final Region region = new Region();
    public final Model model;
    public Output state;

    public Function(Model model) {
        this.model = model;
        this.state = new StateOutput(model.getUnitType());
    }

    public void declareVar(Variable var) {
        variables.add(var);
    }

    public void compile(Block block) {
        Scope scope = new Scope(this);
        Builder builder = new Builder(region, scope);
        state = builder.process(state, block);
    }
}
