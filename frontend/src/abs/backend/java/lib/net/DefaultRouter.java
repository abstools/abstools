/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import java.util.HashMap;
import java.util.Map;

import abs.backend.java.lib.runtime.ABSObject;
import abs.backend.java.lib.runtime.COG;

public class DefaultRouter implements Router {
    private final Map<ABSObject, RouteEntry> nodeForObject = new HashMap<ABSObject, RouteEntry>();
    private final Map<COG, RouteEntry> nodeForCOG = new HashMap<COG, RouteEntry>();
    
    @Override
    public Node getNextNode(Msg m) {
        return null;
    }

    private static class RouteEntry {
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
}
