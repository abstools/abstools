/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ABSThreadManager {
    private static Logger logger = Logging.getLogger(ABSThreadManager.class.getName());
    private final List<ABSThread> threads = new ArrayList<>();
    private final ABSRuntime runtime;
    ABSThreadManager(ABSRuntime r) {
        runtime = r;
    }

    public synchronized void addThread(ABSThread t) {
        threads.add(t);
        if (logger.isLoggable(Level.FINEST)) logger.finest("Added thread "+t);
    }

    public synchronized void removeThread(ABSThread t) {
        threads.remove(t);
        if (logger.isLoggable(Level.FINEST)) logger.finest("Removed thread "+t);
        if (threads.isEmpty()) {
            runtime.systemFinished();
        }
    }

    public synchronized void shutdownAllThreads() {
        for (ABSThread t : threads) {
            t.shutdown();
        }
    }

    public synchronized <T> List<T> getAllCopyOf(Class<T> clazz) {
        List<T> result = new ArrayList<>();
        for (ABSThread t : threads) {
            if (t.getClass().equals(clazz)) {
                result.add((T)t);
            }
        }
        return result;
    }
}
