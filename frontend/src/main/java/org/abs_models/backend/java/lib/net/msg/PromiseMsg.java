/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net.msg;

import org.abs_models.backend.java.lib.net.Promise;
import org.abs_models.backend.java.lib.net.NetCOG;
import org.abs_models.backend.java.lib.runtime.ABSObject;
import org.abs_models.backend.java.lib.types.ABSValue;

public class PromiseMsg implements ObjectTargetMsg {
    public final ABSObject target;
    public final Promise promise;
    public final ABSValue value;
    
    public PromiseMsg(Promise p, ABSObject target, ABSValue value) {
        this.promise = p;
        this.target = target;
        this.value = value;
    }

    @Override
    public ABSObject getTarget() {
        return target;
    }

    @Override
    public NetCOG getCOG() {
	return (NetCOG) getTarget().getCOG();
    }
}
