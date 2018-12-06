/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.locationtypes.infer;

import org.abs_models.frontend.analyser.SemanticError;

public class LocationInferenceError extends SemanticError {

    public LocationInferenceError() {
    }
    
    @Override 
    public String getMsg() {
        return getMessage();
    }
    
    @Override
    public String getMessage() {
        return "Location Type Inference failed, could not find a valid typing";
    }

    @Override
    public int getColumn() {
        return 0;
    }

    @Override
    public int getLine() {
        return 0;
    }
    
    @Override
    public String getFileName() {
        return "";
    }
}
