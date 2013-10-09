package costabs.exceptions;

public class CostabsException extends Exception{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public CostabsException(String message){
		super(message);
	}

	public CostabsException(String message, Throwable exception) {
		super(message, exception);
	}
	
}
