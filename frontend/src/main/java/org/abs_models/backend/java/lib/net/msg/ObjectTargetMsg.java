/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net.msg;

import org.abs_models.backend.java.lib.net.NetCOG;
import org.abs_models.backend.java.lib.runtime.ABSObject;

public interface ObjectTargetMsg extends Msg {
    public ABSObject getTarget();
    public NetCOG getCOG();
}
