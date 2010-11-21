package abs.frontend.typechecker;

import abs.frontend.analyser.TypeError;

public class TypeCheckerException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    private final TypeError typeError;

    public TypeCheckerException(TypeError error) {
        super(error.getHelpMessage());
        this.typeError = error;
    }

    public TypeError getTypeError() {
        return typeError;
    }

}
