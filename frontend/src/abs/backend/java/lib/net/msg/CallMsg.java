/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net.msg;

import abs.backend.java.lib.runtime.ABSObject;

public class CallMsg implements Msg {
    public final ABSObject target;
    public final ABSObject source;
    
    public CallMsg(ABSObject target, ABSObject source) {
        this.target = target;
        this.source = source;
    }
}
