package org.abs_models.frontend.typechecker.nullable;

import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.typechecker.TypeCheckerException;

public class NullCheckerException extends TypeCheckerException {
    public NullCheckerException(TypeError error) {
        super(error);
    }
}
