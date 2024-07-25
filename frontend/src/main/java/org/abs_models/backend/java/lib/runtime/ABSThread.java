/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.logging.Logger;

public abstract class ABSThread extends Thread {
    private static Logger logger = Logging.getLogger(ABSThread.class.getName());
    private static final ThreadLocal<ABSThread> currentThread = new ThreadLocal<>();
    private COG cog;
    private ABSThreadManager manager;
    protected boolean shutdown;

    private void init(ABSThreadManager m) {
        this.manager = m;
        manager.addThread(this);
        setName("ABS Main Thread");
    }

    public ABSThread(ABSThreadManager m) {
        super();
        init(m);
    }

    public COG getCOG() {
        return cog;
    }

    public void setCOG(COG c) {
        cog = c;
    }

    public static ABSThread getCurrentThread() {
        return currentThread.get();
    }

    public static COG getCurrentCOG() {
        final ABSThread thread = ABSThread.getCurrentThread();
        if (thread == null) return null;
        else return thread.getCOG();
    }

    public static Task<?> getCurrentTask() {
        COG cog = ABSThread.getCurrentCOG();
        if (cog == null) return null;
        else return cog.getScheduler().getActiveTask();
    }

    /**
     * This method must be called by subclasses, or subclasses must
     * set the thread-local value by themselves.
     */
    public void run() {
        currentThread.set(this);
    }

    /**
     * Must be called by subclasses at the end of their run method
     */
    protected void finished() {
        currentThread.remove();
        manager.removeThread(this);
    }

    public abstract void checkGuard();

    public synchronized boolean isShutdown() {
        return shutdown;
    }

    public synchronized void shutdown() {
        logger.fine("Thread "+this.threadId()+" received shutdown signal");
        shutdown = true;
        this.interrupt();
    }

    public synchronized void wasInterrupted(InterruptedException e) {
        if (!shutdown) {
            e.printStackTrace();
        }
    }
}
