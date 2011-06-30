/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

public class DefaultRouteEntry implements RouteEntry {
    private static final int DEFAULT_HOPS = 1;
    private final NetNode nextNode;
    private final int hops;
        
    public DefaultRouteEntry(NetNode nextNode) {
	this.nextNode = nextNode;
	hops = DEFAULT_HOPS;
    }

    public DefaultRouteEntry(NetNode nextNode, int hops) {
	this.nextNode = nextNode;
	this.hops = hops;
    }

    @Override
    public int getHops() {
	return hops;
    }
        
    @Override
    public NetNode getNextNode() {
	return nextNode;
    }

    @Override
    public int compareTo(RouteEntry routeEntry) {
	return 0;
    }

    @Override 
    public boolean equals(Object o) {
	return false;
    }
    
    @Override
    public int hashCode() {
	return 4;
    }
}
