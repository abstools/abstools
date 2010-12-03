package abs.frontend.typechecker.locationtypes;

import abs.frontend.analyser.TypeError;
import abs.frontend.typechecker.TypeCheckerException;

public class LocationTypeCheckerException extends TypeCheckerException {

    public LocationTypeCheckerException(TypeError error) {
        super(error);
    }

}
