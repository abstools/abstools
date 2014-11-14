package abs.api;

import java.util.concurrent.Future;

/**
 * A context provides a set of interfaces that allows regulating
 * messages among actor references. The ingredient parts of a context
 * are:
 * <ul>
 * <li>an {@link Router} responsible to route the messages to their
 * recipients. Routing of a message starts from
 * {@link Actor#ask(Actor, Object)} that delegates to its bound context.
 * <li>a {@link Notary} that acts as a registry of actor references and
 * object in the concurrent system.
 * <li>possibly a set of {@link Inbox} instances each of which (or
 * mutually) maintain a place holder to send the messages to the owning
 * recipients.
 * <li>at least one {@link Opener} that eventually is expected to open
 * the sent message and allow the future result to be used.
 * </ul>
 * 
 * <p>
 * <b>Note</b> that the above ingredients are not expected to be
 * existent at all times as different implementations of a context may
 * decide to bypass them.
 * 
 * @see Router
 * @see Notary
 * @see Inbox
 * @see Opener
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface Context extends Lifecycle {

	/**
	 * Creates an instance of {@link Actor} with the provided name and
	 * the object that the reference binds to. It is left to the
	 * implementation if to throw an exception on duplicate situations
	 * or not.
	 * 
	 * @param name
	 *            the name of the new actor reference
	 * @param object
	 *            the binding object for the actor reference
	 * @return the newly created actor reference or possibly an
	 *         unchecked exception
	 */
	Actor newActor(String name, Object object);

	/**
	 * Provides the context's router instance.
	 * 
	 * @return the router instance that is used by the context
	 */
	Router router();

	/**
	 * Provides the context's notary instance.
	 * 
	 * @return the notary instance that is used by the context
	 */
	Notary notary();

	/**
	 * Identifies the inbox "related" to the provided reference.
	 * 
	 * @param reference
	 *            the reference for whom the inbox is needed
	 * @return an instance of inbox that is related/assigned to the
	 *         reference or {@code null} if there is no such inbox
	 */
	Inbox inbox(Reference reference);

	/**
	 * Identifies the opener with which the messages for the recipient
	 * reference can be opened.
	 * 
	 * @param reference
	 *            the reference for whom the openener is needed
	 * @return an instance of envelope opener that can open messages for
	 *         the reference. If {@code null} is returned, it is not
	 *         known how messages will be opened for the reference in
	 *         question.
	 */
	Opener opener(Reference reference);

	/**
	 * Identifies a reference with which a target binding object is
	 * registered in this context. It is a typical implementation to use
	 * {@link Notary} for this purpose but it can be overriden.
	 * 
	 * @param object
	 *            the target binding object in question
	 * @return a reference instance that is registered for the object;
	 *         otherwise, {@code null} if no reference can be found.
	 */
	default Reference reference(Object object) {
		return notary().get(object);
	}
	
	/**
	 * Resolves an object by a given reference.
	 * 
	 * @param <T>
	 *            the type of the object that is expected to be
	 *            registered with the reference
	 * @param reference
	 *            the reference of the object
	 * @return the object resolved in the context or {@code null} if not
	 *         registered. This may also lead to an exception of
	 *         {@link ClassCastException} if the wrong is expected.
	 */
	default <T> T object(Reference reference) {
		return (T) notary().get(reference);
	}
	
	/**
	 * A facility method that allows to send a message to an actor
	 * without being in a context or an actor. The sender of the message
	 * will be {@link Actor#NOBODY} which means that if the recipient
	 * actor uses {@link Actor#sender()}, it leads an exception because
	 * the sender is not known.
	 * 
	 * @see Actor#send(Reference, Object)
	 * 
	 * @param to
	 *            the recipient actor reference
	 * @param message
	 *            the message
	 * @param <V>
	 *            the parameter type that defines the result type of the
	 *            message
	 * @return the result of the message as a future
	 */
	default <V> Future<V> send(Reference to, Object message) {
		return Actor.NOBODY.send(to, message);
	}

}