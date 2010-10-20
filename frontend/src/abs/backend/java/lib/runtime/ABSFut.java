package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSBuiltInDataType;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.FutObserver;
import abs.backend.java.observing.FutView;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskView;


public class ABSFut<V extends ABSValue> extends ABSBuiltInDataType {
    private static final Logger log = Logger.getLogger(ABSRuntime.class.getName());
    private static final AtomicInteger counter = new AtomicInteger();
    private final Task<?> resolvingTask;
    private V value;
    private boolean isResolved;
    private final int id = counter.incrementAndGet();
    
    public ABSFut(Task<?> task) {
        this("Fut", task);
    }
    
    public int getID() {
        return id;
    }
    
	protected ABSFut(String constructorName, Task<?> task) {
        super(constructorName);
        resolvingTask = task;
    }

	
	public void resolve(final V o) {
	    synchronized (this) {
	        if (isResolved)
	            throw new IllegalStateException("Future is already resolved");
	    
	        if (Logging.DEBUG) log.finest(this+" is resolved to "+o);
	    
		    value = o;
		    isResolved = true;
	        notifyAll();
	    }

	    informWaitingThreads();
	    
	    View v = view;
        if (v != null)
            v.onResolved(o);
	}
	
   
   private synchronized void informWaitingThreads() {
   	if (waitingThreads == null)
   		return;
 	  for (ABSThread s : waitingThreads) {
 		  s.checkGuard();
 	  }
 	  waitingThreads.clear();
 }

	
	public synchronized V getValue() {
		return value;
	}
	
	public synchronized boolean isResolved() {
	   return isResolved;
   }

   public synchronized void await() {
   	while (!isResolved) {
   		try {
   			wait();
   		} catch (InterruptedException e) {
   			e.printStackTrace();
   		}
   	}
   }

   @SuppressWarnings("unchecked")
   public V get() {
       synchronized (this) {
           if (!isResolved() && resolvingTask.getCOG() == ABSRuntime.getCurrentCOG())
               throw new ABSDeadlockException();
       }
       
       Task<?> t = ABSRuntime.getCurrentTask();
       if (t != null) {
           t.calledGetOnFut(this);
       }
       
       if (Config.GLOBAL_SCHEDULING) {
           // note that this code does only work in the presence of global scheduling,
           // otherwise it would not be thread-safe
           if (!isResolved()) {
               ABSRuntime.doNextStep();
           }
       }
       
       await();

       if (t != null) {
           t.futureReady(this);           
       }
       
       return value;
   }


   @Override
   public ABSBool eq(ABSValue other) {
	   return ABSBool.fromBoolean(other == this);
   }
   
   public synchronized String toString() {
       return "Future of "+resolvingTask+" ("+(isResolved ? value : "unresolved")+")";
   }

   private volatile View view;
   public synchronized FutView getView() {
       if (view == null) {
           view = new View();
       }
       return view;
   }
   
   private class View implements FutView {

       private List<FutObserver> futObserver;

       private synchronized List<FutObserver> getObservers() {
           if (futObserver == null) 
               futObserver = new ArrayList<FutObserver>(1);
           return futObserver;
       }
       
       synchronized void onResolved(ABSValue v) {
           for (FutObserver f : getObservers()) {
               f.onResolved(this, v);
           }
       }
       
    @Override
    public TaskView getResolvingTask() {
        return resolvingTask.getView();
    }

    @Override
    public boolean isResolved() {
        return ABSFut.this.isResolved();
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


   public synchronized Task<?> getResolvingTask() {
   	return resolvingTask;
   }

   private List<ABSThread> waitingThreads;
	public synchronized void addWaitingThread(ABSThread thread) {
		if (waitingThreads == null)
			waitingThreads = new ArrayList<ABSThread>(1);
		waitingThreads.add(thread);
   }
   
}
