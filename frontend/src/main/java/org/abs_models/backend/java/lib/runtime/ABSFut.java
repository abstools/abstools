/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.lib.types.ABSBuiltInDataType;
import org.abs_models.backend.java.observing.FutObserver;
import org.abs_models.backend.java.observing.FutView;
import org.abs_models.backend.java.observing.TaskView;
import org.abs_models.backend.java.scheduling.GuardWaiter;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

/**
 * The ABS Future datatype.
 */
public abstract class ABSFut<V> extends ABSBuiltInDataType
    implements Future<V>
{
    protected static final Logger log = Logging.getLogger(ABSFut.class.getName());
    private static final AtomicInteger counter = new AtomicInteger();
    private final int id = counter.incrementAndGet();
    /**
     * True if the future is resolved, false if not.
     */
    protected boolean isDone = false;
    /**
     * The value of the resolved future.  Not used if the future was resolved
     * via an exception.
     */
    protected V value = null;
    /**
     * The exception that was returned instead of a value.  Not used if the
     * future was resolved normally.
     */
    protected ABSException exception = null;
    /**
     * List of guards waiting for the future to be resolved.  Note: not used
     * in all schedulers.
     */
    private List<GuardWaiter> waitingThreads = null;

    /**
     * Tri-state flag: null if no thread is waiting, false when at least one
     * thread is inside `wait`, true if at least one thread has woken up (and
     * signaled its cog).
     */
    protected AtomicBoolean taskHasWokenUp = null;

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
     *
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
        log.fine(() -> this + (isDone ? " ready, skipping await" : " awaiting."));

        boolean neededSuspend = !isDone;

        if (neededSuspend) {
            if (taskHasWokenUp == null) {
                taskHasWokenUp = new AtomicBoolean(false);
            }
            log.finest(() -> this + " notifying COG: will suspend.");
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

        if (neededSuspend) {
            log.finest(() -> this + " notifying COG: became ready.");
            cog.notifyWakeup(task);
            taskHasWokenUp.set(true);
            this.notify();
        }
        // TODO: fix this; exceptions should be thrown by get, not by await
        if (exception != null)
            throw exception;
    }

    public void resolve(final V o) {
        resolve(o,null);
    }

    protected void resolve(final V o, final ABSException e) {
        if (isDone)
            throw new IllegalStateException("Future is already resolved");
        synchronized (this) {
            if (isDone)
                throw new IllegalStateException("Future is already resolved");
            value = o;
            exception = e;
            isDone = true;
            this.notifyAll();
            if (taskHasWokenUp != null) {
                log.finest(() -> this + " waiting for at least one awaiting task to wake up");
                while (!taskHasWokenUp.get()) {
                    try {
                        this.wait();
                    } catch (InterruptedException e1) {
                        log.finest(() -> this + " was interruped during await");
                        Thread.currentThread().interrupt();
                    }
                }
            }
        }
        log.finest(() -> this + (e == null
            ? (" is resolved to value " + o)
            : (" is resolved to exception " + e)));
        informWaitingThreads();

        View v = view;
        if (v != null)
            v.onResolved(o);
    }

    public void smash(ABSException e) {
        resolve(null,e);
    }



    private void informWaitingThreads() {
        final ArrayList<GuardWaiter> copy;
        synchronized (this) {
            if (waitingThreads == null)
            {
                log.finest(() -> this + ": no threads waiting for result");
                return;
            }
            copy = new ArrayList<>(waitingThreads);
            waitingThreads.clear();
        }

        log.finest(() -> this + " informing " + copy.size() + " awaiting thread(s)");
        for (GuardWaiter s : copy) {
            s.checkGuard();
        }
    }

    @Override
    public synchronized String toString() {
        return "Future (" + (isDone ? value : "unresolved") + ")";
    }

    public boolean addWaitingThread(GuardWaiter thread) {
        // NOTE: this method is not used in the default scheduler.
        if (isDone) {
            log.finest(() -> this + " is already resolved");
            return false;
        }
        synchronized(this) {
            if (isDone) {
                log.finest(() -> this + " is already resolved");
                return false;
            }
            if (waitingThreads == null)
                waitingThreads = new ArrayList<>(1);
            waitingThreads.add(thread);
        }
        log.finest(() -> "Added guard to queue of " + this);
        return true;
    }


    protected View view;

    public FutView getView() {
        if (view == null) {
            synchronized(this) {
                if (view == null) {
                    view = createView();
                }
            }
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

        synchronized void onResolved(Object v) {
            for (FutObserver f : getObservers()) {
                f.onResolved(this, v);
            }
        }


        @Override
        public TaskView getResolvingTaskView() {
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
        public Object getValue() {
            return ABSFut.this.getValue();
        }

        @Override
        public void registerFutObserver(FutObserver obs) {
            getObservers().add(obs);
        }

    }

}
