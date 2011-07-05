/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import abs.backend.java.lib.net.msg.Msg;
import abs.backend.java.lib.runtime.ABSObject;

public class DefaultRouter implements Router {
    private final Map<ABSObject, RouteEntry> routeForObject = new HashMap<ABSObject, RouteEntry>();
    private final Map<NetCOG, RouteEntry> routeForCOG = new HashMap<NetCOG, RouteEntry>();
    private final NetNode node;

    public DefaultRouter(NetNode node) {
	this.node = node;
    }

    @Override 
    public void update(Router adjacentNodeRouter) {
	// TODO
    }

    @Override
    public void register(ABSObject localObject) {
	if (routeForObject.containsKey(localObject)) {
	    throw new IllegalArgumentException("object " + localObject + " already registered");
	}
	routeForObject.put(localObject, new DefaultRouteEntry(node, 0));
    }

    @Override
    public void register(NetCOG localCOG) {
	if (routeForCOG.containsKey(localCOG)) {
	    throw new IllegalArgumentException("COG " + localCOG + " already registered");
	}
	routeForCOG.put(localCOG, new DefaultRouteEntry(node, 0));
    }
    
    @Override
    public void replace(NetCOG cog, NetNode nextNode, int hops) {
	routeForCOG.put(cog, new DefaultRouteEntry(nextNode, hops));
    }

    @Override
    public void replace(ABSObject object, NetNode nextNode, int hops) {
	routeForObject.put(object, new DefaultRouteEntry(nextNode, hops));
    }
    
    @Override
    public NetNode getNextNode(Msg m) {
        return null; // TODO
    }

    @Override
    public RouteEntry getRouteEntry(NetCOG cog) {
	return routeForCOG.get(cog);
    }

    @Override
    public RouteEntry getRouteEntry(ABSObject object) {
	return routeForObject.get(object);
    }

    @Override
    public Set<ABSObject> getRegisteredObjects() {
	return routeForObject.keySet();
    }

    @Override
    public Set<NetCOG> getRegisteredCOGs() {
	return routeForCOG.keySet();
    }

}
