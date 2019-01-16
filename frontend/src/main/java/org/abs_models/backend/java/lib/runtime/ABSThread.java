/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.logging.Logger;

public class ABSThread extends Thread {
    private static Logger logger = Logging.getLogger(ABSThread.class.getName());
    private COG cog;
    private ABSThreadManager manager;
    protected boolean shutdown;
    
    public ABSThread(Runnable r, ABSThreadManager m) {
        super(r);
        init(m);
    }

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
    

    @Override
    public void run() {
        super.run();
        finished();
    }
    
    /**
     * Must be called by subclasses that override run,
     * when the run method has finished
     */
    protected void finished() {
        manager.removeThread(this);
    }
    

    public void checkGuard() {
    }

    public synchronized boolean isShutdown() {
        return shutdown;
    }
    
    public synchronized void shutdown() {
        logger.fine("Thread "+this.getId()+" received shutdown signal");
        shutdown = true;
        this.interrupt();
    }
    
    public synchronized void wasInterrupted(InterruptedException e) {
        if (!shutdown) {
            e.printStackTrace();
        }
    }
}
