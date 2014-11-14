package abs.api;

/**
 * A general factory that creates an {@link Object} using a fully
 * qualified class name (FQCN).
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface Factory {

	/**
	 * Factory method. This property should when an object is returned:
	 * 
	 * <pre>
	 * object.getClass() == Class.forName(fqcn)
	 * </pre>
	 * 
	 * @param fqcn
	 *            the fully qualified class name of the object to be
	 *            created.
	 * @param ctorArguments
	 *            the constructor arguments for the object to create.
	 * @return the created object or an exception if creation fails such
	 *         as {@link ClassNotFoundException} or arguments mismatch
	 *         or invalid arguments.
	 */
	Object create(String fqcn, String... ctorArguments);

	/**
	 * Checks whether is a factory for a specific class.
	 * 
	 * @param clazz
	 *            the class to be checked
	 * @return {@code true} if this factory supports creating objects
	 *         with the class provided
	 */
	boolean supports(Class<?> clazz);

}
