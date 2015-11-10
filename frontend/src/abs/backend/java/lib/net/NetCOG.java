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
import abs.backend.java.lib.runtime.ABSObject;
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
    private final Map<Promise, ABSFut<? super ABSValue>> futureMap 
        = new HashMap<Promise, ABSFut<? super ABSValue>>();

    public NetCOG(NetNode node, ABSNetRuntime runtime, Class<?> clazz) {
        super(runtime, clazz, null);
        this.node = node;
    }
    
    synchronized void processMsg(Msg msg) {
        if (msg instanceof CallMsg) {
            CallMsg cm = (CallMsg) msg;
            // FIXME: create task and replace promises with futures
            Task<?> task = new Task(cm.call); 
            addTask(task);
        } else {
            PromiseMsg pm = (PromiseMsg) msg;
            promises.put(pm.promise, pm.value);
            ABSFut<? super ABSValue> f = futureMap.get(pm.promise);
            if (f != null) {
                f.resolve(pm.value);
            }
        }
    }

    public synchronized void setNode(NetNode node) {
        this.node = node;
    }
    
    public synchronized NetNode getNode() {
        return node;
    }

    public synchronized void registerFuture(NetFut<? super ABSValue> fut) {
        if (futureMap.containsKey(fut.getPromise()))
            throw new IllegalStateException("Future for promises already existed");
        
        futureMap.put(fut.getPromise(), fut);
        ABSValue v = promises.get(fut.getPromise());
        if (v != null) {
            fut.resolve(v);
            return;
        }
    }
    
    @Override
    public void register(ABSObject absObject) {
        getNode().registerObject(absObject);
    }

}
