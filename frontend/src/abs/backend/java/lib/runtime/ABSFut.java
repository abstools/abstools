package abs.backend.java.lib.runtime;

import java.util.logging.Logger;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSBuiltInDataType;
import abs.backend.java.lib.types.ABSValue;


public class ABSFut<V> extends ABSBuiltInDataType {
    private static final Logger log = Logger.getLogger(ABSRuntime.class.getName());
    private final Task<V> resolvingTask;
    private V value;
    private boolean isResolved;
    
    public ABSFut(Task<V> task) {
        this("Fut", task);
    }
    
	protected ABSFut(String constructorName, Task<V> task) {
        super(constructorName);
        resolvingTask = task;
    }

	
	public synchronized void resolve(V o) {
	    if (isResolved)
	        throw new IllegalStateException("Future is already resolved");
	    
	    if (Logging.DEBUG) log.finest(this+" is resolved to "+o);
	    
		value = o;
		isResolved = true;
		notifyAll();
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

   public synchronized V get() {
       if (!isResolved() && resolvingTask.getCOG() == ABSRuntime.getCurrentCOG())
           throw new ABSDeadlockException();
       
       await();
   	   return value;
   }


   @Override
   public ABSBool eq(ABSValue other) {
	   return ABSBool.fromBoolean(other == this);
   }
   
   public synchronized String toString() {
       return "Future of "+resolvingTask+" ("+(isResolved ? value : "unresolved")+")";
   }
   
}
