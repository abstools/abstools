/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import java.util.HashMap;
import java.util.HashSet;
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
    private NetNode node;
    private final Map<Promise, ABSValue> promises = new HashMap<Promise, ABSValue>();
    private final Map<Promise, Set<ABSFut<? super ABSValue>>> futureMap 
        = new HashMap<Promise, Set<ABSFut<? super ABSValue>>>();

    public NetCOG(NetNode node, ABSNetRuntime runtime, Class<?> clazz) {
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
            // not needed anymore as resolved now
            futureMap.remove(pm.promise);
        }
    }

    public synchronized void setNode(NodeImpl node) {
        this.node = node;
    }
    
    public synchronized NetNode getNode() {
        return node;
    }

    public synchronized void registerFuture(NetFut<? super ABSValue> fut) {
        ABSValue v = promises.get(fut.getPromise());
        if (v != null) {
            fut.resolve(v);
            return;
        }
        
        Set<ABSFut<? super ABSValue>> set = futureMap.get(fut.getPromise());
        if (set == null) {
            set = new HashSet<ABSFut<? super ABSValue>>();
            futureMap.put(fut.getPromise(), set);
        }
        set.add(fut);
    }

}
