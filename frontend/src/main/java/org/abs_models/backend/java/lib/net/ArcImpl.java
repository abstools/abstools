/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net;

import org.abs_models.backend.java.lib.net.msg.MsgQueue;

/**
 * Describes an Arc in a network
 * 
 * @author Jan Schäfer
 *
 */
public class ArcImpl {
    private final NetNode source;
    private final NetNode target;
    private final MsgQueue queue;
    
    public ArcImpl(NetNode source, NetNode target, MsgQueue queue) {
        this.source = source;
        this.target = target;
        this.queue = queue;
    }

    public NetNode getSource() {
        return source;
    }

    public NetNode getTarget() {
        return target;
    }

    public MsgQueue getQueue() {
        return queue;
    }
    
    
}
