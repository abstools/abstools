/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net;

import java.util.Set;
import java.util.List;
import java.util.Map;

import org.abs_models.backend.java.lib.net.msg.Msg;
import org.abs_models.backend.java.lib.runtime.ABSObject;

public interface NetNode {
    public void performStep();
    public void processMsg(Msg msg);
    public void addInArcs(List<ArcImpl> arcs);
    public void addOutArcs(Map<NetNode, ArcImpl> arcs);
    public void registerObject(ABSObject absObject);
    public void registerCOG(NetCOG cog);
    public Set<ABSObject> getRegisteredObjects();
    public Set<NetCOG> getRegisteredCOGs();
    public int getId();
    public NetNode defaultRoute();
}
