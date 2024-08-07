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
import org.abs_models.backend.java.lib.expr.BinOp;
import org.abs_models.backend.java.lib.types.ABSBool;
import org.abs_models.backend.java.lib.types.ABSInterface;
import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.backend.java.observing.COGView;
import org.abs_models.backend.java.observing.ObjectCreationObserver;
import org.abs_models.backend.java.observing.TaskSchedulerView;
import org.abs_models.backend.java.scheduling.TaskScheduler;
import org.abs_models.backend.java.scheduling.TaskSchedulingStrategy;

public class COG implements ABSValue {
    protected static final Logger log = Logging.getLogger(COG.class.getName());

    private final TaskScheduler scheduler;
    private final Class<?> initialClass;
    private final int id;
    private ABSInterface dc;
    /**
     * The number of active threads, incremented and decremented by schedulers
     * running on this cog.  This is used to detect when all schedulers on
     * this cog have gone idle, and hence, time could be incremented.
     * <p>
     * NOTE: all access to this field must be synchronized
     */
    private int activeThreads = 0;

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

    public void addTask(Task<?> task) {
        synchronized(this) {
            if (activeThreads == 0) {
                log.finest(() -> this + " notifying runtime that it became active");
                ABSRuntime.getRuntime().notifyCogActive();
            }
            activeThreads++;
        }
        log.finest(() -> this + " now has " + activeThreads + " active threads.");
        scheduler.addTaskToScheduler(task);
    }

    /**
     * Notify the cog that a guard is awaiting and will suspend the thread.
     * If the guard evaluates to true, this method should not be called (e.g.,
     * if a DurationGuard awaits on t=0).
     */
    public synchronized void notifyAwait(Task<?> task) {
        activeThreads--;
        if (activeThreads < 0) {
            log.severe(() -> this + " reached negative value for activeThreads (" + activeThreads + "), this should never happen");
            throw new IllegalStateException("activeThreads counter reached negative value; this should never happen");
        } else {
            log.finest(() -> this + " now has " + activeThreads + " active threads.");
        }
        if (activeThreads == 0) {
            log.finest(() -> this + " notifying runtime that it became inactive -- all tasks suspended");
            ABSRuntime.getRuntime().notifyCogInactive();
        } else if (scheduler.getActiveTask() == task) {
            // If we await, the active task is either null or another task; if
            // we block, the active task is the one calling `notifyAwait`.
            log.finest(() -> this + " notifying runtime that it became inactive -- active task is blocked");
            ABSRuntime.getRuntime().notifyCogInactive();
        }
    }

    /**
     * Notify the cog that a guard has finished awaiting and the thread is
     * runnable again.
     * <p>
     * If the guard did not actually suspend the task, this method should not
     * be called (e.g., if a DurationGuard awaited on t=0).
     */
    public synchronized void notifyWakeup(Task<?> task) {
        if (activeThreads == 0 || scheduler.getActiveTask() == task) {
            // If we just woke up but are already the active task, we were the
            // task that blocked.
            log.finest(() -> this + " notifying runtime that it became active");
            ABSRuntime.getRuntime().notifyCogActive();
        }
        activeThreads++;
        log.finest(() -> this + " now has " + activeThreads + " active threads.");
    }

    public synchronized void notifyEnded() {
        activeThreads--;
        if (activeThreads < 0) {
            log.severe(() -> this + " reached negative value for activeThreads (" + activeThreads + "), this should never happen");
            throw new IllegalStateException("activeThreads counter reached negative value; this should never happen");
        } else {
            log.finest(() -> this + " now has " + activeThreads + " active threads.");
        }
        if (activeThreads == 0) {
            log.finest(() -> this + " notifying runtime that it became inactive");
            ABSRuntime.getRuntime().notifyCogInactive();
        }
    }

    public int getID() {
        return id;
    }

    public String toString() {
        return "COG [" + initialClass.getSimpleName() + "] (" + getID() + ")";
    }

    public void objectCreated(ABSObject absObject) {
        if (view != null)
            view.objectCreated(absObject);
    }

    public void objectInitialized(ABSObject absObject) {
        if (view != null)
            view.objectInitialized(absObject);
    }

    private View view;

    public synchronized COGView getView() {
        if (view == null) {
            view = new View();
        }
        return view;
    }

    private class View implements COGView {
        private List<ObjectCreationObserver> creationListeners;
        private Map<String, List<ObjectCreationObserver>> creationClassListeners;

        synchronized void notifyListeners(ABSObject absObject, boolean created) {
            if (creationListeners != null) {
                for (ObjectCreationObserver l : creationListeners) {
                    if (created)
                        l.objectCreated(absObject.getView());
                    else
                        l.objectInitialized(absObject.getView());
                }
            }

            if (creationClassListeners != null) {
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

        }

        synchronized void objectCreated(ABSObject absObject) {
            notifyListeners(absObject, true);
        }

        synchronized void objectInitialized(ABSObject absObject) {
            notifyListeners(absObject, false);
        }

        @Override
        public synchronized void registerObjectCreationListener(ObjectCreationObserver listener) {
            if (creationListeners == null) {
                creationListeners = new ArrayList<>(1);
            }
            creationListeners.add(listener);
        }

        @Override
        public synchronized void registerObjectCreationListener(String className, ObjectCreationObserver e) {
            if (creationClassListeners == null) {
                creationClassListeners = new HashMap<>();
            }

            List<ObjectCreationObserver> list
                = creationClassListeners.computeIfAbsent(className, k -> new ArrayList<>(1));
            list.add(e);
        }

        @Override
        public TaskSchedulerView getScheduler() {
            return scheduler.getView();
        }

        @Override
        public int getID() {
            return id;
        }

    }

    public void register(ABSObject absObject) {
        if (ABSDCMirror.CLASS_DC.isInstance(absObject)) {
            log.finest(() -> "registering fresh DC " + absObject + " with runtime");
            ABSRuntime.getRuntime().registerDC((ABSInterface)absObject);
        }
    }

    public Object toJson() {
        throw new RuntimeException("Trying to serialize a Cog; this should never happen");
    }

}
