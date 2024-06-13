package org.abs_models.backend.rvsdg.core;

import org.abs_models.frontend.typechecker.Type;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A gamma node model conditionals with symmetric split and joins, such as if-then-else.
 *
 * It has multiple subregions (called "branch regions").
 * One of these are executed depending on the value of the predicate.
 *
 * Typically the predicate is a boolean and there are two branch regions.
 * The first region then always represents the false-case.
 */
public class GammaNode extends Node {
    public ArrayList<Region> branchRegions = new ArrayList<>();

    public GammaNode(Region region, int regionCount, Output predicate) {
        super(region);

        for (int i = 0; i < regionCount; i++) {
            branchRegions.add(new Region());
        }

        addSimpleInput(predicate);
    }

    public Input getPredicate() {
        return inputs.get(0);
    }

    public GammaOutput createOutput(Type type, List<? extends Output> branchOutputs) {
        assert branchOutputs.size() == branchRegions.size();
        GammaOutput output = new GammaOutput(type, this, outputs.size(), branchOutputs);
        outputs.add(output);
        return output;
    }

    public GammaOutput createOutput(Type type, Output... inputs) {
        return createOutput(type, Arrays.asList(inputs));
    }

    public GammaInput transfer(Output output) {
        GammaInput input = new GammaInput(this, output, this.inputs.size());
        this.inputs.add(input);
        return input;
    }

    public int getBranchCount() {
        return branchRegions.size();
    }
}
