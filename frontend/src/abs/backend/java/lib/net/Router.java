/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import java.util.Set;

import abs.backend.java.lib.runtime.ABSObject;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.net.msg.Msg;

public interface Router {
    
    /**
     * Update the router with routes from a router from an adjacent node
     */
    public void update(Router adjacentNodeRouter);

    /**
     * Register an object that is local to the node
     */
    public void register(ABSObject localObject);

    /**
     * Register a COG that is local to the node
     */
    public void register(COG localCOG);

    /**
     * Replace a route for a COG
     * @param nextNode the node that messages to the COG should be routed to; must be either the current node or a node adjacent to the current node
     * @param hops the number of hops needed to reach the object given that a message is routed to the given node 
     */
    public void replace(COG cog, Node nextNode, int hops);

    /**
     * Replace a route for an object
     * @param nextNode the node that messages to the object should be routed to; must be either the current node or a node adjacent to the current node
     * @param hops the number of hops needed to reach the object given that a message is routed to the given node 
     */     
    public void replace(ABSObject object, Node nextNode, int hops);

    /**
     * Get the node a message should be routed to
     */
    public Node getNextNode(Msg m);

    /**
     * Get all route entries
     */
    public Set<RouteEntry> getRouteEntries();

}

