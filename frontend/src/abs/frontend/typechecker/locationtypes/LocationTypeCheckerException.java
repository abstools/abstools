/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.locationtypes;

import abs.frontend.analyser.TypeError;
import abs.frontend.typechecker.TypeCheckerException;

public class LocationTypeCheckerException extends TypeCheckerException {

    public LocationTypeCheckerException(TypeError error) {
        super(error);
    }

}
