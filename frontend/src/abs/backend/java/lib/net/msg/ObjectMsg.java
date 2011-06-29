/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net.msg;

import abs.backend.java.lib.runtime.ABSObject;

public class ObjectMsg implements Msg {
    public final ABSObject object;
    public ObjectMsg(ABSObject o) {
        this.object = o;
    }
}
