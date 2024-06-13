package org.abs_models.backend.rvsdg.abs;

import org.abs_models.backend.rvsdg.core.Node;
import org.abs_models.backend.rvsdg.core.Output;
import org.abs_models.backend.rvsdg.core.Region;
import org.abs_models.frontend.ast.DataConstructor;

public class DataConstructNode extends Node {
    public DataConstructor dataConstructor;

    public DataConstructNode(Region region, DataConstructor dataConstructor) {
        super(region);
        this.dataConstructor = dataConstructor;

        addSimpleOutput(dataConstructor.getType());
    }

    public void addParam(Output value) {
        addSimpleInput(value);
    }

    public Output getResult() {
        return outputs.get(0);
    }
}
