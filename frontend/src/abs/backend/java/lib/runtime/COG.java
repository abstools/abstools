/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectCreationObserver;
import abs.backend.java.observing.TaskSchedulerView;
import abs.backend.java.scheduling.DefaultTaskScheduler;
import abs.backend.java.scheduling.SimpleTaskScheduler;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.TaskSchedulingStrategy;

public class COG implements ABSValue {
    private final TaskScheduler scheduler;
    private final Class<?> initialClass;
    private final int id;
    private final ABSRuntime runtime;
    private abs.backend.java.lib.types.ABSInterface dc;

    public COG(ABSRuntime runtime, Class<?> clazz, abs.backend.java.lib.types.ABSInterface dc) {
        initialClass = clazz;
        this.runtime = runtime;
        scheduler = runtime.createTaskScheduler(this);
        id = runtime.freshCOGID();
        this.dc = dc;
    }

    public COG(ABSRuntime runtime, Class<?> clazz, abs.backend.java.lib.types.ABSInterface dc, TaskSchedulingStrategy schedulingStrategy) {
        initialClass = clazz;
        this.runtime = runtime;
        scheduler = runtime.createUserTaskScheduler(this, schedulingStrategy);
        id = runtime.freshCOGID();
        this.dc = dc;
    }
    
    public ABSRuntime getRuntime() {
        return runtime;
    }

    public Class<?> getInitialClass() {
        return initialClass;
    }

    public TaskScheduler getScheduler() {
        return scheduler;
    }

    public abs.backend.java.lib.types.ABSInterface getDC() {
        return dc;
    }

    public void addTask(Task<?> task) {
        scheduler.addTask(task);
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
                creationListeners = new ArrayList<ObjectCreationObserver>(1);
            }
            creationListeners.add(listener);
        }

        @Override
        public synchronized void registerObjectCreationListener(String className, ObjectCreationObserver e) {
            if (creationClassListeners == null) {
                creationClassListeners = new HashMap<String, List<ObjectCreationObserver>>();
            }

            List<ObjectCreationObserver> list = creationClassListeners.get(className);
            if (list == null) {
                list = new ArrayList<ObjectCreationObserver>(1);
                creationClassListeners.put(className, list);
            }
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
        // nothing to do
    }

    @Override
    public ABSBool eq(ABSValue o) {
        return ABSBool.fromBoolean(this.equals(o));
    }

    @Override
    public ABSBool notEq(ABSValue o) {
        return eq(o).negate();
    }

    @Override
    public ABSBool gt(ABSValue other) {
        if (other.getClass() == COG.class) {
            return ABSBool.fromBoolean(this.id > ((COG)other).getID());
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    @Override
    public ABSBool lt(ABSValue other) {
        if (other.getClass() == COG.class) {
            return ABSBool.fromBoolean(this.id < ((COG)other).getID());
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    @Override
    public ABSBool gtEq(ABSValue other) {
        if (other.getClass() == COG.class) {
            return eq(other).or(gt(other));
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    @Override
    public ABSBool ltEq(ABSValue other) {
        if (other.getClass() == COG.class) {
            return eq(other).or(lt(other));
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    @Override
    public boolean isDataType() {
        return false;
    }

    @Override
    public boolean isReference() {
        return true;
    }

}
