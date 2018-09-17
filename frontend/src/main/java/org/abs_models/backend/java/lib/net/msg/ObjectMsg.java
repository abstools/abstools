/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net.msg;

import org.abs_models.backend.java.lib.runtime.ABSObject;
import org.abs_models.backend.java.lib.net.NetCOG;

public class ObjectMsg implements Msg {
    private final ABSObject object;

    public ObjectMsg(ABSObject object) {
        this.object = object;
    }

    public ABSObject getObject() {
	return object;
    }

    public NetCOG getCOG() {
	return (NetCOG) object.getCOG();
    }
}
