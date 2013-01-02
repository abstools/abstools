/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.List;

import abs.backend.java.lib.types.ABSRef;
import abs.backend.java.lib.types.ABSValue;

public interface AsyncCall<T extends ABSRef> {

    Object execute();
    
    String methodName();
    
    List<ABSValue> getArgs();
    
    T getTarget();
    
    ABSObject getSource();

    Task<?> getSender();
    
}