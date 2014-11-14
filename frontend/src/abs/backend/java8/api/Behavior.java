package abs.api;

/**
 * A behavior is an abstraction to introduce a generic top-level
 * interface for any object to turn into a behavior providing a way to
 * respond to a general object message. Behavior is a functional
 * interface although it's not marked with {@link FunctionalInterface}
 * .
 * 
 * <p>
 * The interface can be used in {@link Actor#ask(Actor, Object)}
 * knowing that the receiver of the message is a behavior and can
 * respond to a general message.
 * 
 * <p>
 * A behavior introduces generality of an actor but loses the static
 * type checking of possible arguments of a message.
 * 
 * @see Actor
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface Behavior {

	/**
	 * Responds to a general message. The type and structure of the
	 * message is to be determined by the implementation. The actors
	 * that depend on a behavior for the outgoing message should make
	 * sure they use the appropriate message types sent that are
	 * expected by the recipient.
	 * 
	 * @param message
	 *            the message object to be processed by the recipient
	 * @return the result of processing of the message; by default
	 *         implementation it is <code>null</code>
	 */
	default Object respond(Object message) {
		return null;
	}

}
