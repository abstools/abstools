/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net.msg;

import java.util.concurrent.ConcurrentLinkedQueue;


/**
 * An unbounded message queue
 *
 * @author Jan Schäfer
 *
 */
public class UnboundedMsgQueue implements MsgQueue {
    private final ConcurrentLinkedQueue<Msg> queue = new ConcurrentLinkedQueue<>();


    @Override
    public void enqueue(Msg m) {
        queue.add(m);
    }

    @Override
    public Msg dequeue() {
        return queue.remove();
    }

    @Override
    public boolean isEmpty() {
        return queue.isEmpty();
    }

}
