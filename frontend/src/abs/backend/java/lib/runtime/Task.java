package abs.backend.java.lib.runtime;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.lib.types.ABSRef;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.FutView;
import abs.backend.java.observing.ObjectCreationListener;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.TaskListener;
import abs.backend.java.observing.TaskView;

public abstract class Task<T extends ABSRef> {
    private static AtomicInteger counter = new AtomicInteger();
    private final ABSFut<? super ABSValue> future;
    protected final T target;
    private final int id = counter.incrementAndGet();
    
    public Task(T target) {
        this.target = target;
        future = new ABSFut(this);
    }
    
    public COG getCOG() {
        return ((ABSObject)target).getCOG();        
    }
    
    public void schedule() {
        getCOG().addTask(this);
    }
    
    public ABSFut<?> getFut() {
        return future;
    }
    
    public void run() {
        synchronized (this) {
            if (view != null)
                view.taskStarted();
        }
        
        ABSValue res = (ABSValue) execute();
        future.resolve(res);
        
        synchronized (this) {
            if (view != null)
                view.taskFinished();
        }
        
    }
    
    public abstract Object execute();
    
    public abstract String methodName();
    
    public String toString() {
        return "Task ("+id+") ["+getCOG()+", Method: "+target.getClass().getSimpleName()+"."+methodName()+"]";
    }

    private View view;
    public synchronized TaskView getView() {
        if (view == null) { 
            view = new View();
        }
        return view;
    }
    
    private class View implements TaskView {
        private List<TaskListener> taskListener;

        
        @Override
        public ObjectView getTarget() {
            return ((ABSObject)target).getView();
        }

        private synchronized List<TaskListener> getListener() {
            if (taskListener == null) 
                taskListener = new ArrayList<TaskListener>(1);
            return taskListener;
        }
        
        public synchronized void taskStarted() {
            for (TaskListener l : getListener()) {
                l.taskStarted(this);
            }
        }

        public synchronized void taskFinished() {
            for (TaskListener l : getListener()) {
                l.taskFinished(this);
            }
        }

        @Override
        public COGView getCOG() {
            return Task.this.getCOG().getView();
        }

        @Override
        public String getMethodName() {
            return Task.this.methodName();
        }

        @Override
        public List<ABSValue> getArgs() {
            return Task.this.getArgs();
        }

        @Override
        public FutView getFuture() {
            return future.getView();
        }

        @Override
        public synchronized void registerTaskListener(TaskListener listener) {
            getListener().add(listener);
        }
        
    }

    private List<ABSValue> getArgs() {
        List<ABSValue> result = new ArrayList<ABSValue>();
        for (Field f : getClass().getDeclaredFields()) {
            try {
                ABSValue v = (ABSValue) f.get(this);
                result.add(v);
            } catch (IllegalArgumentException e) {
                e.printStackTrace();
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            }
        }
        return result;
    }
}
