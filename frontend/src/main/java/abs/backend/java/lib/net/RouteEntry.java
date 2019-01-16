/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

public interface RouteEntry extends Comparable<RouteEntry> {

    public int getHops();
        
    public NetNode getNextNode();

}
