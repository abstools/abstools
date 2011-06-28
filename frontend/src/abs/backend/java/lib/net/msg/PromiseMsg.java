/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net.msg;

import abs.backend.java.lib.runtime.ABSObject;

public class PromiseMsg implements Msg {
    public final ABSObject target;
    // public final Promise promise;
    // public final Value value;
    
    public PromiseMsg(ABSObject target) {
        this.target = target;
    }
}
