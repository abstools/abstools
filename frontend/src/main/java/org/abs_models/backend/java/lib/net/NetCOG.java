/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net;

import java.util.HashMap;
import java.util.Map;

import org.abs_models.backend.java.lib.net.msg.CallMsg;
import org.abs_models.backend.java.lib.net.msg.Msg;
import org.abs_models.backend.java.lib.net.msg.PromiseMsg;
import org.abs_models.backend.java.lib.runtime.ABSFut;
import org.abs_models.backend.java.lib.runtime.ABSObject;
import org.abs_models.backend.java.lib.runtime.COG;
import org.abs_models.backend.java.lib.runtime.Task;

/**
 * A NET-aware COG
 *
 */
public class NetCOG extends COG {
    private NetNode node;
    private final Map<Promise, Object> promises = new HashMap<>();
    private final Map<Promise, ABSFut<Object>> futureMap
        = new HashMap<>();

    public NetCOG(NetNode node, ABSNetRuntime runtime, Class<?> clazz) {
        super(runtime, clazz, null);
        this.node = node;
    }

    synchronized void processMsg(Msg msg) {
        switch (msg) {
            case CallMsg cm:
                // FIXME: create task and replace promises with futures
                Task<?> task = new Task<>(cm.call);
                addTask(task);
                break;
            case PromiseMsg pm:
                promises.put(pm.promise, pm.value);
                ABSFut<Object> f = futureMap.get(pm.promise);
                if (f != null) {
                    f.resolve(pm.value);
                }
                break;
            default:
            // nothing to do
        }
    }

    public synchronized void setNode(NetNode node) {
        this.node = node;
    }

    public synchronized NetNode getNode() {
        return node;
    }

    public synchronized void registerFuture(NetFut<Object> fut) {
        if (futureMap.containsKey(fut.getPromise()))
            throw new IllegalStateException("Future for promises already existed");

        futureMap.put(fut.getPromise(), fut);
        Object v = promises.get(fut.getPromise());
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
