/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

public class DefaultRouteEntry implements RouteEntry {
    private static final int DEFAULT_HOPS = 1;
    private final NodeImpl nextNode;
    private final int hops;
        
    public DefaultRouteEntry(NodeImpl nextNode) {
	this.nextNode = nextNode;
	hops = DEFAULT_HOPS;
    }

    public DefaultRouteEntry(NodeImpl nextNode, int hops) {
	this.nextNode = nextNode;
	this.hops = hops;
    }

    @Override
    public int getHops() {
	return hops;
    }
        
    @Override
    public NodeImpl getNextNode() {
	return nextNode;
    }
}
