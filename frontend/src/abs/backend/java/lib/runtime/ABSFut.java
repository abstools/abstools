/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSBuiltInDataType;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.FutObserver;
import abs.backend.java.observing.FutView;
import abs.backend.java.observing.TaskView;
import abs.backend.java.scheduling.GuardWaiter;

public abstract class ABSFut<V extends ABSValue> extends ABSBuiltInDataType {
    protected static final Logger log = Logger.getLogger(ABSRuntime.class.getName());
    private static final AtomicInteger counter = new AtomicInteger();
    private final int id = counter.incrementAndGet();
    protected V value;
    protected ABSException exception;
    protected boolean isResolved;

    protected ABSFut() {
        super("Fut");
    }

    public int getID() {
        return id;
    }


    public abstract V get();

    public synchronized V getValue() {
        return value;
    }

    public synchronized boolean isResolved() {
        return isResolved;
    }

    public synchronized void await() {
        log.finest("awaiting future");

        while (!isResolved) {
            try {
                wait();
            } catch (InterruptedException e) {
                log.finest("was interruped during await");
                Thread.currentThread().interrupt();
                break;
            }
        }


        if (exception != null)
            throw exception;

        log.finest("future ready");
    }

    private List<GuardWaiter> waitingThreads;
    public void resolve(final V o) {
        resolve(o,null);
    }

    protected void resolve(final V o, final ABSException e) {
        synchronized (this) {
            if (isResolved)
                throw new IllegalStateException("Future is already resolved");

            log.finest(this + " is resolved to " + o);

            value = o;
            exception = e;
            isResolved = true;
            notifyAll();
        }

        informWaitingThreads();

        View v = view;
        if (v != null)
            v.onResolved(o);

    }

    public void smash(ABSException e) {
        resolve(null,e);
    }



    private void informWaitingThreads() {
        log.finest(this + " inform awaiting threads...");

        ArrayList<GuardWaiter> copy = null;
        synchronized (this) {
            if (waitingThreads == null)
                return;
            copy = new ArrayList<GuardWaiter>(waitingThreads);
            waitingThreads.clear();
        }

        for (GuardWaiter s : copy) {
            s.checkGuard();
        }
    }


    @Override
    public ABSBool eq(ABSValue other) {
        return ABSBool.fromBoolean(other == this);
    }


    @Override
    public ABSBool gt(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!(o instanceof ABSFut))
            return ABSBool.FALSE;
        return ABSBool.fromBoolean(getID() > ((ABSFut)o).getID());
    }

    @Override
    public ABSBool lt(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!(o instanceof ABSFut))
            return ABSBool.FALSE;
        return ABSBool.fromBoolean(getID() < ((ABSFut)o).getID());
    }

    @Override
    public ABSBool gtEq(ABSValue o) {
        if (this.eq(o).toBoolean())
            return ABSBool.TRUE;
        else
            return this.gt(o);
    }

    @Override
    public ABSBool ltEq(ABSValue o) {
        if (this.eq(o).toBoolean())
            return ABSBool.TRUE;
        else
            return this.lt(o);
    }


    @Override
    public synchronized String toString() {
        return "Future (" + (isResolved ? value : "unresolved") + ")";
    }

    public synchronized boolean addWaitingThread(GuardWaiter thread) {
        if (isResolved) {
            log.fine("===== "+this+" is already resolved");
            return false;
        }
        if (waitingThreads == null)
            waitingThreads = new ArrayList<GuardWaiter>(1);
        waitingThreads.add(thread);
        return true;
    }


    protected volatile View view;

    public synchronized FutView getView() {
        if (view == null) {
            view = createView();
        }
        return view;
    }

    protected View createView() {
        return new View();
    }

    protected class View implements FutView {

        private List<FutObserver> futObserver;

        private synchronized List<FutObserver> getObservers() {
            if (futObserver == null)
                futObserver = new ArrayList<FutObserver>(1);
            return futObserver;
        }

        synchronized void onResolved(ABSValue v) {
            for (FutObserver f : getObservers()) {
                f.onResolved(this, v);
            }
        }


        @Override
        public TaskView getResolvingTask() {
            return null;
        }

        @Override
        public boolean isResolved() {
            return ABSFut.this.isResolved();
        }

        @Override
        public int getID() {
            return ABSFut.this.getID();
        }

        @Override
        public ABSValue getValue() {
            return ABSFut.this.getValue();
        }

        @Override
        public void registerFutObserver(FutObserver obs) {
            getObservers().add(obs);
        }

    }

}
