package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSBuiltInDataType;
import abs.backend.java.lib.types.ABSDataType;


public class ABSFut extends ABSBuiltInDataType {
    
    
    public ABSFut() {
        this("Fut");
    }
    
	protected ABSFut(String constructorName) {
        super(constructorName);
    }

    private Object value;
	private boolean isResolved;
	
	public synchronized void resolve(Object o) {
		value = o;
		isResolved = true;
	}
	
	public synchronized Object getValue() {
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

   public synchronized Object get() {
   	return value;
   }


   @Override
   public ABSBool eq(ABSDataType other) {
	   return ABSBool.fromBoolean(other == this);
   }
   
}
