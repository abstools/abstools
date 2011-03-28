package abs.backend.java.fli;

public @interface RealizesABSClass {

	/**
	 * The ABS class as a full qualified name.
	 * e.g.: Test.SomeClass
	 * @return the ABS class as a full qualified name
	 */
	String value();
	
}
