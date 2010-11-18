package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.lib.types.ABSRef;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.FutView;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskView;

public abstract class Task<T extends ABSRef> {
    private static AtomicInteger counter = new AtomicInteger();
    private final ABSFut<? super ABSValue> future;
    protected final T target;
    protected final ABSObject source;
    protected final Task<?> sender;
    private final int id = counter.incrementAndGet();
    private Thread executingThread;
    private ABSException exception;
    
    public Task(ABSObject source, T target) {
        this(ABSRuntime.getCurrentTask(),source,target);
    }

    public Task(Task<?> sender, ABSObject source, T target) {
        this.sender = sender;
        this.source = source;
        this.target = target;
        future = new ABSFut(this);
    }
    
    public int getID() {
        return id;
    }

    public synchronized boolean isDeadlocked() {
        return exception != null && exception.isDeadlock();
    }

    public synchronized boolean hasException() {
        return exception != null;
    }
    
    public synchronized ABSException getException() {
        return exception;
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
    
    // only for observing
    void calledGetOnFut(ABSFut<?> someFut) {
        View v = view;
        if (v != null)
            v.calledGetOnFut(someFut);
    }

    // only for observing
    void futureReady(ABSFut<?> someFut) {
        View v = view;
        if (v != null)
            v.futureReady(someFut);
    }
    
    public void run() {
        synchronized (this) {
            if (view != null)
                view.taskStarted();
            executingThread = Thread.currentThread();
        }
        
        try {
            ABSValue res = (ABSValue) execute();
            future.resolve(res);
        } catch (ABSException e) {
            this.exception = e;
            System.err.println("Error in "+this+":\n"+e.getMessage());
        } catch (Exception e) {
            e.printStackTrace();
        }
        
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

    private volatile View view;
    public synchronized TaskView getView() {
        if (view == null) { 
            view = new View();
        }
        return view;
    }
    
    protected ABSValue[] getArgs() {
        return new ABSValue[0];
    }

    public synchronized void nextStep(String fileName, int line) {
        if (view != null)
            view.nextStep(fileName, line);
    }
    
    
    private class View implements TaskView {
        private List<TaskObserver> taskListener;

        @Override
        public TaskView getSender() {
            if (sender == null) return null;
            return sender.getView();
        }

        public synchronized void nextStep(String fileName, int line) {
            for (TaskObserver l : getObservers()) {
                l.taskStep(this,fileName,line);
            }
        }

        public synchronized void futureReady(ABSFut<?> someFut) {
            for (TaskObserver l : getObservers()) {
                l.taskRunningAfterWaiting(this, someFut.getView());
            }
        }

        public synchronized void calledGetOnFut(ABSFut<?> someFut) {
            for (TaskObserver l : getObservers()) {
                l.taskBlockedOnFuture(this, someFut.getView());
            }
        }

        @Override
        public ObjectView getSource() {
            if (source == null) return null;
            return source.getView();
        }
        
        @Override
        public ObjectView getTarget() {
            return ((ABSObject)target).getView();
        }

        private synchronized List<TaskObserver> getObservers() {
            if (taskListener == null) 
                taskListener = new ArrayList<TaskObserver>(1);
            return taskListener;
        }
        
        public synchronized void taskStarted() {
            for (TaskObserver l : getObservers()) {
                l.taskStarted(this);
            }
        }

        public synchronized void taskFinished() {
            for (TaskObserver l : getObservers()) {
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
            return Arrays.asList(Task.this.getArgs());
        }

        @Override
        public FutView getFuture() {
            return future.getView();
        }
        
        @Override
        public int getID() {
            return id;
        }
        
        @Override
        public boolean isDeadlocked() {
            return Task.this.isDeadlocked();
        }
        
        @Override
        public synchronized void registerTaskListener(TaskObserver listener) {
            getObservers().add(listener);
        }

        @Override
        public boolean hasException() {
            return Task.this.hasException();
        }

        @Override
        public ABSException getException() {
            return Task.this.getException();
        }
        
    }


	public synchronized Thread getExecutingThread() {
	   return executingThread;
   }


}
