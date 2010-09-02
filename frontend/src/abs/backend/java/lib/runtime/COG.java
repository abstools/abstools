package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectCreationListener;
import abs.backend.java.observing.SchedulerView;
import abs.backend.java.observing.TaskListener;
import abs.backend.java.observing.TaskView;

public class COG {
	private TaskScheduler scheduler = new TaskScheduler(this);
	private Class<?> initialClass;
	private static AtomicInteger counter = new AtomicInteger();
	private final int id = counter.incrementAndGet();

	public COG(Class<?> clazz) {
	    initialClass = clazz;
	}
	
	public TaskScheduler getScheduler() {
	   return scheduler;
    }

	public void release() {
		
	}
	
	public void aquire() {
		
	}
	
	public void addTask(Task task) {
	    scheduler.addTask(task);
	}

    public int getID() {
        return id;
    }
    
    public String toString() {
        return "COG ["+initialClass.getSimpleName()+"] ("+getID()+")";
    }

    public void objectCreated(ABSObject absObject) {
        if (view != null)
            view.objectCreated(absObject);
    }
    
    private View view;
    public synchronized COGView getView() {
        if (view == null) {
            view = new View();
        }
        return view;
    }
    
    private class View implements COGView {
        private List<ObjectCreationListener> creationListeners;
        private Map<String, List<ObjectCreationListener>> creationClassListeners;
        
        
        synchronized void objectCreated(ABSObject absObject) {
            if (creationListeners != null) {
                for (ObjectCreationListener l : creationListeners) {
                    l.objectCreated(absObject.getView(), false);
                }
            }
            
            if (creationClassListeners != null) {
                List<ObjectCreationListener> list = creationClassListeners.get(absObject.getClassName());
                if (list != null) {
                    for (ObjectCreationListener l : list) {
                        l.objectCreated(absObject.getView(), false);
                    }
                }
            }
        }

        @Override
        public synchronized void registerObjectCreationListener(
                ObjectCreationListener listener) {
            if (creationListeners == null) {
                creationListeners = new ArrayList<ObjectCreationListener>(1);
            }
            creationListeners.add(listener);
        }

        @Override
        public synchronized void registerObjectCreationListener(String className,
                ObjectCreationListener e) {
            if (creationClassListeners == null) {
                creationClassListeners = new HashMap<String, List<ObjectCreationListener>>();
            }
            
            List<ObjectCreationListener> list = creationClassListeners.get(className);
            if (list == null) {
                list = new ArrayList<ObjectCreationListener>(1);
                creationClassListeners.put(className,list);
            }
            list.add(e);
        }

        @Override
        public SchedulerView getScheduler() {
            return scheduler.getView();
        }

        
        
    }

}
