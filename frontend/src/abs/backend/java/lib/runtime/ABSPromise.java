/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.net.Promise;
import abs.backend.java.lib.types.ABSBuiltInDataType;
import abs.backend.java.lib.types.ABSValue;

public class ABSPromise<V extends ABSValue> extends ABSBuiltInDataType {

    public final Promise promise;

    protected ABSPromise(Promise promise, String constructorName) {
        super("Promise");
        this.promise = promise;
    }
    
    
    
}
