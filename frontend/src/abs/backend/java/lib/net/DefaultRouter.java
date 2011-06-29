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
import abs.backend.java.lib.runtime.COG;

public class DefaultRouter implements Router {
    private final Map<ABSObject, RouteEntry> nodeForObject = new HashMap<ABSObject, RouteEntry>();
    private final Map<COG, RouteEntry> nodeForCOG = new HashMap<COG, RouteEntry>();

    private final Node node;

    public DefaultRouter(Node node) {
	this.node = node;
    }

    @Override 
    public void update(Router adjacentNodeRouter) {
	// find new routes and replace if better than current ones  
    }

    @Override
    public void register(ABSObject localObject) {
	// register the object so that messages should be routed to the current node with 0 hops
    }

    @Override
    public void register(COG localCOG) {
	// register the COG so that messages should be routed to the current node with 0 hops
    }
    
    @Override
    public void replace(COG cog, Node nextNode, int hops) {
	// replace current route entry for cog with new entry 
    }

    @Override
    public void replace(ABSObject object, Node nextNode, int hops) {
	// replace current route entry for object with new entry 
    }
    
    @Override
    public Node getNextNode(Msg m) {
        return null;
    }

    @Override
    public RouteEntry getRouteEntry(COG cog) {
	return null;
    }

    @Override
    public RouteEntry getRouteEntry(ABSObject object) {
	return null;
    }

    @Override
    public Set<ABSObject> getRegisteredObjects() {
	return null;
    }

    @Override
    public Set<COG> getRegisteredCOGs() {
	return null;
    }

}
