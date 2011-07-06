/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import java.util.Set;

import abs.backend.java.lib.runtime.ABSObject;
import abs.backend.java.lib.net.msg.Msg;

public interface Router {
    
    /**
     * Update the router with routes from a router from an adjacent node
     */
    public void update(NetNode adjacentNode, Router adjacentNodeRouter);

    /**
     * Register an object that is local to the node
     */
    public void register(ABSObject localObject);
    
    /**
     * Register a COG that is local to the node
     */
    public void register(NetCOG localCOG);

    /**
     * Replace a route for a COG or register it
     * @param nextNode the node that messages to the COG should be routed to; must be either the current node or a node adjacent to the current node
     * @param hops the number of hops needed to reach the object given that a message is routed to the given node 
     */
    public void replace(NetCOG cog, NetNode nextNode, int hops);

    /**
     * Replace a route for an object or register it
     * @param nextNode the node that messages to the object should be routed to; must be either the current node or a node adjacent to the current node
     * @param hops the number of hops needed to reach the object given that a message is routed to the given node 
     */     
    public void replace(ABSObject object, NetNode nextNode, int hops);

    /**
     * Get the node a message should be routed to
     */
    public NetNode getNextNode(Msg m);

    /**
     * Get the route entry for a cog
     */
    public RouteEntry getRouteEntry(NetCOG cog);

    /**
     * Get the (currently best) route entry for an object
     */
    public RouteEntry getRouteEntry(ABSObject object);

    /**
     * Get all objects tracked by the router
     */
    public Set<ABSObject> getRegisteredObjects();

    /**
     * Get all cogs tracked by the router
     */
    public Set<NetCOG> getRegisteredCOGs();

}
