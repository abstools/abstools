/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.List;
import java.util.concurrent.Callable;

import org.abs_models.backend.java.lib.types.ABSRef;

public interface AsyncCall<T extends ABSRef> extends Callable<Object> {

    Object call();
    
    String methodName();
    
    List<Object> getArgs();
    
    T getTarget();
    
    ABSObject getSource();

    Task<?> getSender();
    
}
