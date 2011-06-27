/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

public class RouteEntry {
    private final Node nextNode;
    private int hops = 1;
    
    public RouteEntry(Node nextNode) {
        this.nextNode = nextNode;
    }
    
    public int getHops() {
        return hops;
    }
    
    public void setHops(int i) {
        hops = i;
    }

    public Node getNextNode() {
        return nextNode;
    }
    
    
}
