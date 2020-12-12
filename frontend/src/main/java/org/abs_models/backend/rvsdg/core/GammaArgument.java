package org.abs_models.backend.rvsdg.core;

import org.abs_models.frontend.typechecker.Type;

/**
 * A gamma argument represents a value inside a region that comes from the outer region.
 */
public class GammaArgument extends Output {
    public final GammaInput input;
    public final int regionIdx;

    public GammaArgument(Type type, GammaInput input, int regionIdx) {
        super(type);
        this.input = input;
        this.regionIdx = regionIdx;
    }
}
