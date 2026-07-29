/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.abs_models.backend.java.JavaBackendException;
import org.abs_models.backend.java.lib.types.ABSInterface;
import org.abs_models.backend.java.observing.COGView;
import org.abs_models.backend.java.observing.ObjectCreationObserver;
import org.abs_models.backend.java.observing.TaskSchedulerView;
import org.abs_models.backend.java.scheduling.TaskScheduler;
import org.abs_models.backend.java.scheduling.TaskSchedulingStrategy;

/**
 * This class implements the unit of concurrency of ABS.  Scheduling
 * policy is implemented by a pluggable scheduler stored in the
 * `scheduler` field.
 *
 * The cog is informed by tasks and the scheduler about task state.
 * The Cog in turn informs the `ABSRuntime` singleton about its state;
 * based on all cogs' states the runtime will decide if time can be
 * advanced or the model has finished.
 */
public class COG {
    protected static final Logger log = Logging.getLogger(COG.class.getName());

    private final TaskScheduler scheduler;
    private final Class<?> initialClass;
    private final long id;
    private ABSInterface dc;
    /**
     * The number of currently runnable threads.  This variable is
     * used to detect when all tasks on this cog have gone idle, and
     * hence, time could be incremented.
     *
     * <p> NOTE: all access to this field must be synchronized:
     * depending on the scheduler, each ABS task might run on its own
     * Java thread and multiple tasks can become runnable at the same
     * time.
     */
    private int runnableThreads = 0;

    public COG(ABSRuntime runtime, Class<?> clazz, ABSInterface dc) {
        initialClass = clazz;
        scheduler = runtime.createTaskScheduler(this);
        id = runtime.freshCOGID();
        this.dc = dc;
    }

    public COG(ABSRuntime runtime, Class<?> clazz, ABSInterface dc, TaskSchedulingStrategy schedulingStrategy) {
        initialClass = clazz;
        scheduler = runtime.createUserTaskScheduler(this, schedulingStrategy);
        id = runtime.freshCOGID();
        this.dc = dc;
    }

    public Class<?> getInitialClass() {
        return initialClass;
    }

    public TaskScheduler getScheduler() {
        return scheduler;
    }

    public ABSInterface getDC() {
        return dc;
    }

    /**
     * Set the deployment component of this cog.  NOTE: this method should
     * never be called, except to set the deployment component of the
     * primordial cog in the generated code of a module's main block.
     *
     * @param dc The deployment component of the cog.
     */
    public void setDCfromMainBlock(ABSInterface dc) {
        if (this.dc != null) {
            throw new JavaBackendException("Trying to override the deployment component of a cog");
        }
        this.dc = dc;
    }

    /**
     * Add a task to the cog (and its scheduler), and inform the
     * runtime that the cog became active if necessary.
     */
    public void addTask(Task<?> task) {
        synchronized(this) {
            if (runnableThreads == 0) {
                if (scheduler.getActiveTask() == null) {
                    log.finest(() -> this + " notifying runtime that it became active");
                    ABSRuntime.getRuntime().notifyCogActive(this);
                } else {
                    // a new task came in but we're currently blocked
                    // on a task -- don't tell the runtime we can run
                }
            }
            runnableThreads++;
        }
        log.finest(() -> this + " now has " + runnableThreads + " runnable threads.");
        scheduler.addTaskToScheduler(task);
    }

    /**
     * Notify the cog that a guard is awaiting and will suspend the thread.
     *
     * <p> This method can be called either by the scheduler or by a
     * guard directly.  In the former case, the scheduler will not
     * have an active task ({@code scheduler.getActiveTask()} returns
     * null), which means the task is suspended.  The latter case is
     * {@emph blocking} behavior, called e.g. by future {@code get}
     * and {@code ABSResourceGuard}, and does not involve the
     * scheduler so {@code scheduler.getActiveTask()} will return
     * non-null.
     *
     * <p> If the guard evaluates to true, this method should not be
     * called (e.g., if a DurationGuard awaits on t=0).  In that case,
     * execution should just continue without awaiting at all.
     */
    public synchronized void notifyAwait(Task<?> task) {
        runnableThreads--;
        if (runnableThreads < 0) {
            log.severe(() -> this + " reached negative value for runnableThreads (" + runnableThreads + "), this should never happen");
            throw new IllegalStateException("runnableThreads counter reached negative value; this should never happen");
        } else {
            log.finest(() -> this + " now has " + runnableThreads + " runnable threads.");
        }
        if (runnableThreads == 0) {
            log.finest(() -> this + " notifying runtime that it became inactive -- all tasks suspended");
            ABSRuntime.getRuntime().notifyCogInactive(this);
        } else if (scheduler.getActiveTask() == task) {
            // If we await, the active task is either null or another task; if
            // we block, the active task is the one calling `notifyAwait`.
            log.finest(() -> this + " notifying runtime that it became inactive -- active task is blocked");
            ABSRuntime.getRuntime().notifyCogInactive(this);
        } else {
            // Nothing to do -- we're not blocked and there are
            // runnable tasks; the scheduler will just pick one to
            // run.
        }
    }

    /**
     * Notify the cog that a guard has finished awaiting and the
     * task is runnable again.  Note that this method is called by
     * any task whose guard became true, not just the currently
     * running one.
     *
     * <p> If the guard did not actually suspend the task, this method
     * should not be called (e.g., if a DurationGuard awaited on t=0).
     */
    public synchronized void notifyWakeup(Task<?> task) {
        if (runnableThreads == 0) {
            // all tasks were suspended, but one woke up now
            log.finest(() -> this + " notifying runtime that it became active");
            ABSRuntime.getRuntime().notifyCogActive(this);
        } else if (scheduler.getActiveTask() == task) {
            // the blocked task got unblocked
            log.finest(() -> this + " notifying runtime that it became unblocked");
            ABSRuntime.getRuntime().notifyCogActive(this);
        }
        runnableThreads++;
        log.finest(() -> task + " ready, " + this + " now has " + runnableThreads + " runnable threads.");
    }

    /**
     * Notify the cog that a task has finished executing.
     */
    public synchronized void notifyEnded(Task<?> task) {
        runnableThreads--;
        if (runnableThreads < 0) {
            log.severe(() -> this + " reached negative value for runnableThreads (" + runnableThreads + "), this should never happen");
            throw new IllegalStateException("runnableThreads counter reached negative value; this should never happen");
        } else {
            log.finest(() -> task + " ended, " + this + " now has " + runnableThreads + " runnable threads.");
        }
        if (runnableThreads == 0) {
            log.finest(() -> this + " notifying runtime that it became inactive -- last task finished");
            ABSRuntime.getRuntime().notifyCogInactive(this);
        }
    }

    public long getID() {
        return id;
    }

    public String toString() {
        return "COG [" + initialClass.getSimpleName() + "] (" + getID() + ")";
    }

    public void objectCreated(ABSObject absObject) {
        log.fine(() -> "Object created: " + absObject);
        if (view != null)
            view.objectCreated(absObject);
    }

    public void objectInitialized(ABSObject absObject) {
        log.finer(() -> "Object initialized: " + absObject);
        if (view != null)
            view.objectInitialized(absObject);
    }

    private View view;

    public COGView getView() {
        if (view == null) {
            synchronized(this) {
                if (view == null) {
                    view = new View();
                }
            }
        }
        return view;
    }

    private class View implements COGView {
        private List<ObjectCreationObserver> creationListeners = new ArrayList<>();
        private Map<String, List<ObjectCreationObserver>> creationClassListeners = new HashMap<>();

        synchronized void notifyListeners(ABSObject absObject, boolean created) {
            for (ObjectCreationObserver l : creationListeners) {
                if (created)
                    l.objectCreated(absObject.getView());
                else
                    l.objectInitialized(absObject.getView());
            }

            List<ObjectCreationObserver> list = creationClassListeners.get(absObject.getClassName());
            if (list != null) {
                for (ObjectCreationObserver l : list) {
                    if (created)
                        l.objectCreated(absObject.getView());
                    else
                        l.objectInitialized(absObject.getView());
                }
            }
        }

        synchronized void objectCreated(ABSObject absObject) {
            notifyListeners(absObject, true);
        }

        synchronized void objectInitialized(ABSObject absObject) {
            notifyListeners(absObject, false);
        }

        @Override
        public synchronized void registerObjectCreationListener(ObjectCreationObserver listener) {
            creationListeners.add(listener);
        }

        @Override
        public synchronized void registerObjectCreationListener(String className, ObjectCreationObserver e) {
            List<ObjectCreationObserver> list
                = creationClassListeners.computeIfAbsent(className, k -> new ArrayList<>(1));
            list.add(e);
        }

        @Override
        public TaskSchedulerView getSchedulerView() {
            return scheduler.getView();
        }

        @Override
        public long getID() {
            return id;
        }

        @Override
        public COG getCOG() {
            return COG.this;
        }

    }

    public void register(ABSObject absObject) {
        if (ABSDCMirror.CLASS_DC.isInstance(absObject)) {
            log.finest(() -> "registering fresh DC " + absObject + " with runtime");
            ABSRuntime.getRuntime().registerDC((ABSInterface)absObject);
        }
    }

}
