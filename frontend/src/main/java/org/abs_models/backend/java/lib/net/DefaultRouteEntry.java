/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net;

public class DefaultRouteEntry implements RouteEntry {
    private final NetNode nextNode;
    private final NetNode sourceNode;
    private final int hops;
        
    public DefaultRouteEntry(NetNode nextNode, NetNode sourceNode) {
	this.nextNode = nextNode;
	this.sourceNode = sourceNode;
	hops = 0;
    }

    public DefaultRouteEntry(NetNode nextNode, int hops, NetNode sourceNode) {
	if (hops < 0) {
	    throw new IllegalArgumentException("hops " + hops + " less than 0");
	}
	this.nextNode = nextNode;
	this.sourceNode = sourceNode;
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
    public int compareTo(RouteEntry entry) {
	return hops - entry.getHops();
    }

    @Override 
    public boolean equals(Object o) {
	if (o == this) {
	    return true;
	} else if (!(o instanceof DefaultRouteEntry)) {
	    return false;
	} else {
	    DefaultRouteEntry entry = (DefaultRouteEntry) o;
	    return nextNode == entry.getNextNode() && 
		hops == entry.getHops() && 
		sourceNode == entry.getSourceNode();
	}
    }
    
    @Override
    public int hashCode() {
	int result = 17;
	result = 31 * result + hops;
	result = 31 * result + nextNode.getId();
	result = 31 * result + sourceNode.getId();
	return result;
    }

    public NetNode getSourceNode() {
	return sourceNode;
    }
}
