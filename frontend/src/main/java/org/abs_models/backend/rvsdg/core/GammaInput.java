package org.abs_models.backend.rvsdg.core;

import java.util.ArrayList;
import java.util.List;

/**
 * Used for transferring values from the outer region into the branch regions.
 *
 * @see GammaNode#transfer(Output)
 * */
public class GammaInput extends SimpleInput {
    public final List<GammaArgument> arguments = new ArrayList<>();
    public final GammaNode gammaNode;
    public final int inputIdx;

    public GammaInput(GammaNode gammaNode, Output source, int inputIdx) {
        super(source);
        this.gammaNode = gammaNode;
        this.inputIdx = inputIdx;

        for (int i = 0; i < gammaNode.getBranchCount(); i++) {
            arguments.add(new GammaArgument(source.type, this, i));
        }
    }

    /**
     * Returns an Input which is valid inside one specific branch region.
     *
     * @param i index of the branch region.
     */
    public GammaArgument get(int i) {
        return arguments.get(i);
    }
}
