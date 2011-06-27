/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

/**
 * Representing a Node in the ABS network
 * 
 * @author Jan Schäfer
 *
 */
public class Node {
    private final int id;
    private final Router router = new DefaultRouter();
    private final Map<Node,Arc> outArcs = new HashMap<Node, Arc>();
    private final List<Arc> inArcs = new ArrayList<Arc>();
    
    public Node(int id) {
        this.id = id;
    }
    
    public void processNextMsg() {
        for (Arc arc : inArcs) {
            
        }
    }
}
