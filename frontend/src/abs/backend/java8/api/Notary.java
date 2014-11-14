package abs.api;

/**
 * A notary is a registry authority inside an instance of
 * {@link abs.api.Context} that maintains the record for
 * {@link abs.api.Actor}s and their binding object in the owning
 * context. Notary supports a {@link abs.api.Lifecycle}.
 *
 * @see LocalNotary
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface Notary extends Lifecycle {

	/**
	 * Registers a reference with its binding object.
	 *
	 * @param reference
	 *            the reference for the binding object
	 * @param target
	 *            the target object for which a reference is to be
	 *            registered
	 * @throws java.lang.RuntimeException
	 *             if the registration is not valid; e.g. a duplicate
	 *             entry for the reference
	 */
	void add(Reference reference, Object target) throws RuntimeException;

	/**
	 * Removes the registration of a reference in the notary.
	 *
	 * @param reference
	 *            the reference to be removed
	 * @return {@code true} if the removal is successful; otherwise
	 *         {@code false}
	 */
	boolean remove(Reference reference);

	/**
	 * Identifies a binding object for the provided reference.
	 *
	 * @param reference
	 *            the reference to find a binding object for
	 * @return the registered object in the notary with the reference;
	 *         otherwise, {@code null}
	 */
	Object get(Reference reference);

	/**
	 * Identifies a reference for an object.
	 *
	 * @param object
	 *            the object to find a reference for
	 * @return the registered reference in the notary with which the
	 *         object is registered; otherwise {@code null}
	 */
	Reference get(Object object);

	/**
	 * Finds the actual reference that is identical to the provided
	 * reference.
	 * 
	 * @param reference
	 *            the provided reference
	 * @return the actual referenc or {@code null} if no such reference
	 *         exists.
	 */
	Reference identify(Reference reference);

	/**
	 * Provides the size of the notary in terms of the count of the
	 * object holding.
	 * 
	 * @return the count of the notary
	 */
	long size();

}
