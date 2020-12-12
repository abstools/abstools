package org.abs_models.backend.rvsdg.core;

import org.abs_models.frontend.typechecker.Type;

import java.util.List;

/**
 * The outputs from the gamma node. It's constructed with values from each branch.
 */
public class GammaOutput extends Output {
    final public List<? extends Output> branchOutputs;
    final public GammaNode gammaNode;
    final public int idx;

    public GammaOutput(Type type, GammaNode gammaNode, int idx, List<? extends Output> branchOutputs) {
        super(type);
        this.gammaNode = gammaNode;
        this.idx = idx;

        for (Output branchOutput : branchOutputs) {
            assert branchOutput.type.equals(type);
        }
        this.branchOutputs = branchOutputs;
    }
}
