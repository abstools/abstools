package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectCreationObserver;
import abs.backend.java.observing.TaskSchedulerView;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskView;

public class COG {
    private static final TaskSchedulerFactory schedulerFactory = getSchedulerFactory();
	private TaskScheduler scheduler = schedulerFactory.createTaskScheduler(this);
//    private TaskScheduler scheduler = new DefaultTaskScheduler(this); 
	private Class<?> initialClass;
	private static AtomicInteger counter = new AtomicInteger();
	private final int id = counter.incrementAndGet();

	public COG(Class<?> clazz) {
	    initialClass = clazz;
	}
	
	public Class<?> getInitialClass() {
	    return initialClass;
	}
	
	private static TaskSchedulerFactory getSchedulerFactory() {
	    String schedulerName = System.getProperty("abs.taskscheduler","default");
	    System.out.println("Scheduler: "+schedulerName);
	    if (schedulerName.equals("default"))
            return SimpleTaskScheduler.getFactory();
	    else if (schedulerName.equals("fast"))
            return DefaultTaskScheduler.getFactory();
	    System.err.println("The task scheduler "+schedulerName+" does not exist, falling back to the default task scheduler.");
        return DefaultTaskScheduler.getFactory();
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
        private List<ObjectCreationObserver> creationListeners;
        private Map<String, List<ObjectCreationObserver>> creationClassListeners;
        
        
        synchronized void objectCreated(ABSObject absObject) {
            if (creationListeners != null) {
                for (ObjectCreationObserver l : creationListeners) {
                    l.objectCreated(absObject.getView(), false);
                }
            }
            
            if (creationClassListeners != null) {
                List<ObjectCreationObserver> list = creationClassListeners.get(absObject.getClassName());
                if (list != null) {
                    for (ObjectCreationObserver l : list) {
                        l.objectCreated(absObject.getView(), false);
                    }
                }
            }
        }

        @Override
        public synchronized void registerObjectCreationListener(
                ObjectCreationObserver listener) {
            if (creationListeners == null) {
                creationListeners = new ArrayList<ObjectCreationObserver>(1);
            }
            creationListeners.add(listener);
        }

        @Override
        public synchronized void registerObjectCreationListener(String className,
                ObjectCreationObserver e) {
            if (creationClassListeners == null) {
                creationClassListeners = new HashMap<String, List<ObjectCreationObserver>>();
            }
            
            List<ObjectCreationObserver> list = creationClassListeners.get(className);
            if (list == null) {
                list = new ArrayList<ObjectCreationObserver>(1);
                creationClassListeners.put(className,list);
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
