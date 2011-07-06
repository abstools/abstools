/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import abs.backend.java.lib.net.msg.Msg;
import abs.backend.java.lib.runtime.ABSObject;

public interface NetNode {

    public void performStep();

    public void processMsg(Msg msg);

    public void registerObject(ABSObject absObject);

    public int getId();

    public NetNode defaultRoute();

}
