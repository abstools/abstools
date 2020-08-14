package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.typechecker.TypeCheckerException;

public class LocationTypeInferException extends TypeCheckerException {
    public LocationTypeInferException(TypeError error) {
        super(error);
    }
}
