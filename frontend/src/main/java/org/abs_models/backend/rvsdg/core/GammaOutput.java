package org.abs_models.backend.rvsdg.core;

import org.abs_models.frontend.typechecker.Type;

import java.util.List;

/**
 * The outputs from the gamma node. It's constructed with values from each branch.
 */
public class GammaOutput extends Output {
    final public List<? extends Output> branchOutputs;

    public GammaOutput(Type type, List<? extends Output> branchOutputs) {
        super(type);

        for (Output branchOutput : branchOutputs) {
            assert branchOutput.type.equals(type);
        }
        this.branchOutputs = branchOutputs;
    }
}
