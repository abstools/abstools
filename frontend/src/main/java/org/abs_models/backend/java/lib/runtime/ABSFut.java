/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.lib.types.ABSBool;
import org.abs_models.backend.java.lib.types.ABSBuiltInDataType;
import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.backend.java.observing.FutObserver;
import org.abs_models.backend.java.observing.FutView;
import org.abs_models.backend.java.observing.TaskView;
import org.abs_models.backend.java.scheduling.GuardWaiter;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

public abstract class ABSFut<V extends ABSValue> extends ABSBuiltInDataType
    implements Future<V>
{
    protected static final Logger log = Logging.getLogger(ABSFut.class.getName());
    private static final AtomicInteger counter = new AtomicInteger();
    private final int id = counter.incrementAndGet();
    protected V value;
    protected ABSException exception;
    protected boolean isDone;

    protected ABSFut() {
        super("Fut");
    }

    public int getID() {
        return id;
    }

    /**
     * Minimal implementation of java.concurrent.Future interface.
     *
     * @param _mayInterruptIfRunning {@code true} if the thread executing this
     * task should be interrupted; otherwise, in-progress tasks are allowed
     * to complete
     * @return always false since we currently can't cancel ABS processes.
     */
    public boolean cancel(boolean _mayInterruptIfRunning) {
        return false;
    }

    /**
     * Minimal implementation of java.concurrent.Future interface.
     * @return always false
     */
    public boolean isCancelled() {
        return false;
    }

    public abstract V get();

    /**
     * Minimal implementation of java.concurrent.Future interface.  Currently untested.
     * @param timeout the maximum time to wait
     * @param unit the time unit of the timeout argument
     * @return the value of the future.
     * @throws TimeoutException
     * @throws InterruptedException
     */
    public V get(long timeout, TimeUnit unit) throws TimeoutException, InterruptedException {
        long timeoutMs = unit.toMillis(timeout);
        long startTime = System.currentTimeMillis();
        synchronized (this) {
            // WARNING (rudi): this has not been audited wrt deadlocks against
            // the rest of the Java runtime
            while (!isDone) {
                long elapsedTime = System.currentTimeMillis() - startTime;
                long remainingTime = timeoutMs - elapsedTime;
                if (remainingTime <= 0) {
                    throw new TimeoutException();
                }
                this.wait(remainingTime);
            }
            // if (isCancelled) {
            //     throw new CancellationException();
            // }
            return value;
        }
    }

    public synchronized V getValue() {
        return value;
    }

    public synchronized boolean isDone() {
        return isDone;
    }

    public synchronized void awaitForModelApi() {
        log.finest(() -> this + " awaiting for Model API");

        while (!isDone) {
            try {
                this.wait();
            } catch (InterruptedException e) {
                log.finest(() -> this + " was interruped during await");
                Thread.currentThread().interrupt();
                break;
            }
        }

        log.finest(() -> this + " ready for Model API");
    }

    public synchronized void await(COG cog, Task<?> task) {
        log.finest(() -> this + " awaiting");

        boolean needsSuspend = !isDone;

        if (needsSuspend) {
            cog.notifyAwait(task);
        }

        while (!isDone) {
            try {
                this.wait();
            } catch (InterruptedException e) {
                log.finest(() -> this + " was interruped during await");
                Thread.currentThread().interrupt();
                break;
            }
        }

        log.finest(() -> this + " ready");

        if (needsSuspend) {
            cog.notifyWakeup(task);
        }
        // TODO: fix this; exceptions should be thrown by get, not by await
        if (exception != null)
            throw exception;
    }

    private List<GuardWaiter> waitingThreads;
    public void resolve(final V o) {
        resolve(o,null);
    }

    protected void resolve(final V o, final ABSException e) {
        synchronized (this) {
            if (isDone)
                throw new IllegalStateException("Future is already resolved");

            log.finest(this + " is resolved to " + o);

            value = o;
            exception = e;
            isDone = true;
            this.notifyAll();
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
        log.finest(this + " inform awaiting threads");

        ArrayList<GuardWaiter> copy = null;
        synchronized (this) {
            if (waitingThreads == null)
                return;
            copy = new ArrayList<>(waitingThreads);
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
        return "Future (" + (isDone ? value : "unresolved") + ")";
    }

    public boolean addWaitingThread(GuardWaiter thread) {
        if (isDone) {
            log.fine("===== "+this+" is already resolved");
            return false;
        }
        synchronized(this) {
            if (isDone) {
                log.fine("===== "+this+" is already resolved");
                return false;
            }
            if (waitingThreads == null)
                waitingThreads = new ArrayList<>(1);
            waitingThreads.add(thread);
            return true;
        }
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
                futObserver = new ArrayList<>(1);
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
            return ABSFut.this.isDone();
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

    public Object toJson() {
        return this.toString();
    }
}
