package abs.backend.java.lib.types;


public class ABSFut extends ABSDataType {
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
