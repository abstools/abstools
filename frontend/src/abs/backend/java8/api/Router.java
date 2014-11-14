package abs.api;

/**
 * A router determines the dispatching of messages to an appropriate
 * {@link abs.api.Inbox}. A router is a functional interface.
 *
 * @see LocalRouter
 * @see Inbox
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface Router extends Contextual {

	/**
	 * Ensures that the provided message enveloped is received by an
	 * object that knows how to open it. The object here can be an inbox
	 * or an opener of the envelope and is left to the implementation.
	 *
	 * @param envelope
	 *            the envelope that should be routed
	 */
	void route(Envelope envelope);

}
