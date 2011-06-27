/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

/**
 * Describes an Arc in a network
 * 
 * @author Jan Schäfer
 *
 */
public class Arc {
    private final Node source;
    private final Node target;
    private final MsgQueue queue;
    
    public Arc(Node source, Node target, MsgQueue queue) {
        this.source = source;
        this.target = target;
        this.queue = queue;
    }

    public Node getSource() {
        return source;
    }

    public Node getTarget() {
        return target;
    }

    public MsgQueue getQueue() {
        return queue;
    }
    
    
}
