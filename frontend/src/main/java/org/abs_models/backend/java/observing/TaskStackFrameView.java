/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

import java.util.Set;

import org.abs_models.backend.java.lib.types.ABSValue;

/**
 * Represents a single stack frame of a method invocation.
 * A stack frame is a map from variable names to values.
 * 
 * @author Jan Schäfer
 *
 */
public interface TaskStackFrameView {
    
    /**
     * Returns the stack to which this frame belongs to
     * @return the stack to which this frame belongs to
     */
    public TaskStackView getStack();
    
    /**
     * Returns all variable names of this stack frame
     * @return
     */
    public Set<String> getVariableNames();
    
    /**
     * Returns the value of variable variableName
     * @param variableName the name of the variable to get the value of
     * @return the value of variable variableName
     */
    public ABSValue getValue(String variableName);
    
    /** 
     * Returns the method of this stack frame
     * @return the method of this stack frame
     */
    public MethodView getMethod();

}
