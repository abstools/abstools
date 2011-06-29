/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import abs.backend.java.lib.net.msg.CallMsg;
import abs.backend.java.lib.net.msg.Msg;
import abs.backend.java.lib.net.msg.PromiseMsg;
import abs.backend.java.lib.runtime.ABSFut;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Task;
import abs.backend.java.lib.types.ABSValue;

/**
 * A NET-aware COG
 *
 */
public class NetCOG extends COG {
    private NodeImpl node;
    private final Map<Promise, ABSValue> promises = new HashMap<Promise, ABSValue>();
    private final Map<Promise, Set<ABSFut<? super ABSValue>>> futureMap 
        = new HashMap<Promise, Set<ABSFut<? super ABSValue>>>();

    public NetCOG(NodeImpl node, ABSRuntime runtime, Class<?> clazz) {
        super(runtime, clazz);
        this.node = node;
    }
    
    synchronized void processMsg(Msg msg) {
        if (msg instanceof CallMsg) {
            CallMsg cm = (CallMsg) msg;
            Task<?> task = null; // FIXME: create task and replace promises with futures
            addTask(task);
        } else {
            PromiseMsg pm = (PromiseMsg) msg;
            promises.put(pm.promise, pm.value);
            for (ABSFut<? super ABSValue> f : futureMap.get(pm.promise)) {
                f.resolve(pm.value);
            }
        }
    }

    public synchronized void setNode(NodeImpl node) {
        this.node = node;
    }
    
    @Override
    public void addTask(Task<?> task) {
        node.processMsg(new CallMsg(null, null));
    }

}
