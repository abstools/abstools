/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net;

import org.abs_models.backend.java.lib.runtime.ABSFut;
import org.abs_models.backend.java.lib.types.ABSValue;

public class NetFut<V extends ABSValue> extends ABSFut<V> {

    private final Promise promise;

    public NetFut(Promise promise) {
        this.promise = promise;
    }

    @Override
    public V get() {
        return null;
    }

    public Promise getPromise() {
        return promise;
    }

}
