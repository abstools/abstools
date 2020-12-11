package org.abs_models.backend.rvsdg.core;

import org.abs_models.frontend.typechecker.Type;

/**
 * A gamma argument represents a value inside a region that comes from the outer region.
 */
public class GammaArgument extends Output {
    public final GammaInput input;

    public GammaArgument(Type type, GammaInput input) {
        super(type);
        this.input = input;
    }
}
