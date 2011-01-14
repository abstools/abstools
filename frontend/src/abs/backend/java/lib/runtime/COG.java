package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectCreationObserver;
import abs.backend.java.observing.TaskSchedulerView;
import abs.backend.java.scheduling.DefaultTaskScheduler;
import abs.backend.java.scheduling.SimpleTaskScheduler;
import abs.backend.java.scheduling.TaskScheduler;

public class COG {
    private TaskScheduler scheduler;
    private Class<?> initialClass;
    private static AtomicInteger counter = new AtomicInteger();
    private final int id = counter.incrementAndGet();
    private final ABSRuntime runtime;

    public COG(ABSRuntime runtime, Class<?> clazz) {
        initialClass = clazz;
        this.runtime = runtime;
        scheduler = runtime.createTaskScheduler(this);
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

    public void release() {

    }

    public void aquire() {

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

}
