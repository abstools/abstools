package abs.api;

/**
 * Allows to dynamically bind different objects that require an instance
 * of {@link Context} to an appropriate instance.
 * 
 * @see Context
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface Contextual {

	/**
	 * Binds the object to the provided context.
	 *
	 * @param context
	 *            the context that should be bound for the underlying
	 *            object
	 */
	void bind(Context context);

}
