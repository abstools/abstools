package eu.hatsproject.absplugin.exceptions;

/**
 * This class represents exceptions in a java job 
 * which not maps to the existing exceptions
 * e.g. for user mistakes
 */
public class AbsJobException extends Exception {
	
	/**
	 * a generated serialVersionUId
	 */
	private static final long serialVersionUID = 4390613464722126595L;

	public AbsJobException() {
		super();
	}
	
	public AbsJobException(String message) {
		super(message);
	}

}
