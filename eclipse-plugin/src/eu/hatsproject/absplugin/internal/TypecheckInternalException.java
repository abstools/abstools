package eu.hatsproject.absplugin.internal;

public class TypecheckInternalException extends Exception {

	private static final long serialVersionUID = -3846977590027791843L;
	private Exception wrappedException;
	
	public TypecheckInternalException(Exception wrapped){
		wrappedException = wrapped;
	}
	
	public Exception getWrappedException(){
		return wrappedException;
	}
	
	@Override
	public String toString() {
		return super.toString() + "\nWrapped:\n" + wrappedException.toString();
	}
}
