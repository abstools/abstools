package org.abs_models.backend.rvsdg.core;

import java.util.ArrayList;
import java.util.List;

/**
 * Used for transferring values from the outer region into the branch regions.
 *
 * @see GammaNode#transfer(Output)
 * */
public class GammaInput extends SimpleInput {
    public final List<Output> arguments = new ArrayList<>();

    public GammaInput(GammaNode gammaNode, Output source) {
        super(source);

        for (Region region : gammaNode.branchRegions) {
            arguments.add(new GammaArgument(source.type, this));
        }
    }

    /**
     * Returns an Input which is valid inside one specific branch region.
     *
     * @param i index of the branch region.
     */
    public Output get(int i) {
        return arguments.get(i);
    }
}
