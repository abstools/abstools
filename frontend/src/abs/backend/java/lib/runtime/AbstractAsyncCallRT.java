/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSRef;
import abs.backend.java.lib.types.ABSValue;

public abstract class AbstractAsyncCallRT<T extends ABSRef> extends AbstractAsyncCall<T> implements AsyncCallRTAttributes {

    long deadline;
    long cost;
    boolean critical;

    public AbstractAsyncCallRT(ABSObject source, T target, ABSValue dl, ABSValue co, ABSValue cr) {
        super(source, target);
        deadline = 0;      //TODO: convert ABSValue dl to long
        cost = 0;          //TODO: convert ABSValue co to long
        critical = false;  //TODO: convert ABSValue cr to long
    }
    
    @Override
    public long getDeadline() {
        return deadline;
    }

    @Override
    public long getCost() {
        return cost;
    }

    @Override
    public boolean isCritical() {
        return critical;
    }

}
