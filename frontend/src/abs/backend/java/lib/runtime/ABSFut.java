package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSBuiltInDataType;
import abs.backend.java.lib.types.ABSDataType;


public class ABSFut<V> extends ABSBuiltInDataType {
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
       if (resolvingTask.getCOG() == ABSRuntime.getCurrentCOG())
           throw new ABSDeadlockException();
       
       await();
   	   return value;
   }


   @Override
   public ABSBool eq(ABSDataType other) {
	   return ABSBool.fromBoolean(other == this);
   }
   
}
