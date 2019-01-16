/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import abs.backend.java.lib.net.msg.Msg;
import abs.backend.java.lib.net.msg.ObjectTargetMsg;
import abs.backend.java.lib.net.msg.ObjectMsg;
import abs.backend.java.lib.runtime.ABSObject;

public class DefaultRouter implements Router {
    private final Map<ABSObject, DefaultRouteEntry> routeForObject = new HashMap<>();
    private final Map<NetCOG, DefaultRouteEntry> routeForCOG = new HashMap<>();
    private final NetNode node;

    public DefaultRouter(NetNode node) {
	this.node = node;
    }

    @Override
    public void update(NetNode adjacentNode, Router adjacentNodeRouter) {
	for (ABSObject object : adjacentNodeRouter.getRegisteredObjects()) {
	    RouteEntry otherEntry = adjacentNodeRouter.getRouteEntry(object);
	    DefaultRouteEntry newEntry = new DefaultRouteEntry(adjacentNode, otherEntry.getHops() + 1, adjacentNode);
	    if (routeForObject.containsKey(object)) {
		DefaultRouteEntry currentEntry = routeForObject.get(object);
		if (newEntry.getSourceNode() == currentEntry.getSourceNode() || newEntry.compareTo(currentEntry) < 0) {
		    routeForObject.put(object, newEntry);
		}
	    } else {
		routeForObject.put(object, newEntry);
	    }
	}
	for (NetCOG cog : adjacentNodeRouter.getRegisteredCOGs()) {
	    RouteEntry otherEntry = adjacentNodeRouter.getRouteEntry(cog);
	    DefaultRouteEntry newEntry = new DefaultRouteEntry(adjacentNode, otherEntry.getHops() + 1, adjacentNode);
	    if (routeForCOG.containsKey(cog)) {
		DefaultRouteEntry currentEntry = routeForCOG.get(cog);
		if (newEntry.getSourceNode() == currentEntry.getSourceNode() || newEntry.compareTo(currentEntry) < 0) {
		    routeForCOG.put(cog, newEntry);
		}
	    } else {
		routeForCOG.put(cog, newEntry);
	    }
	}
    }

    @Override
    public void register(ABSObject localObject) {
	if (routeForObject.containsKey(localObject)) {
	    throw new IllegalArgumentException("object " + localObject + " already registered");
	}
	routeForObject.put(localObject, new DefaultRouteEntry(node, node));
    }

    @Override
    public void register(NetCOG localCOG) {
	if (routeForCOG.containsKey(localCOG)) {
	    throw new IllegalArgumentException("COG " + localCOG + " already registered");
	}
	routeForCOG.put(localCOG, new DefaultRouteEntry(node, node));
    }

    @Override
    public void replace(NetCOG cog, NetNode nextNode, int hops) {
	routeForCOG.put(cog, new DefaultRouteEntry(nextNode, hops, node));
    }

    @Override
    public void replace(ABSObject object, NetNode nextNode, int hops) {
	routeForObject.put(object, new DefaultRouteEntry(nextNode, hops, node));
    }

    @Override
    public NetNode getNextNode(Msg m) {
	if (m instanceof ObjectTargetMsg) {
	    ObjectTargetMsg om = (ObjectTargetMsg) m;
	    ABSObject target = om.getTarget();
	    if (routeForObject.containsKey(target)) {
		return routeForObject.get(target).getNextNode();
	    } else {
		return node.defaultRoute();
	    }
	} else if (m instanceof ObjectMsg) {
	    ObjectMsg om = (ObjectMsg) m;
	    NetCOG target = om.getCOG();
	    if (routeForCOG.containsKey(target)) {
		return routeForCOG.get(target).getNextNode();
	    } else {
		return node.defaultRoute();
	    }
	} else {
	    return node.defaultRoute();
	}
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
