package abs.api;

import java.net.URI;
import java.util.concurrent.Callable;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;

/**
 * An actor is a reference that exposes a set of methods to send
 * messages to another actor. There are a number of ways that a message
 * would have meaning as an executable entity in this implementation:
 * <ul>
 * <li>a method invocation exposed by
 * {@link #invoke(Actor, String, Object...)}
 * <li>an instance of {@link Runnable} or {@link Callable} exposed by
 * {@link #ask(Actor, Object)}
 * <li>the recipient of the message is an instance of {@link Behavior}
 * which leads to running {@link Behavior#respond(Object)}
 * </ul>
 * 
 * <p>
 * Every actor is registered with an instance of {@link Context}. A
 * gathers different layers of the actor system to be used by any actor
 * such as routing or executing messages.
 * 
 * <p>
 * This interface in this version exposes methods as {@code ask} which
 * allows to capture the result of the message into a future value.
 * However, to have the model of {@code tell} in actor (fire and
 * forget), simply the result of the message can be ignored.
 * 
 * @see Reference
 * @see MethodReference
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface Actor extends Reference, Comparable<Reference> {

	/**
	 * The prefix for all actors created in a context.
	 */
	String NS = "abs://";

	/**
	 * NOBODY refers to any recipient that is not recognized by its
	 * {@link #name()} in the system.
	 */
	Actor NOBODY = new Actor() {
		private static final long serialVersionUID = 6203481166223651274L;

		private final URI name = URI.create(NS + "NOBODY");

		@Override
		public URI name() {
			return name;
		}

		@Override
		public boolean equals(Object obj) {
			if (obj == null) {
				return false;
			}
			if (obj == this) {
				return true;
			}
			if (obj instanceof Reference == false) {
				return false;
			}
			return name.equals(((Reference) obj).name());
		};

		@Override
		public String toString() {
			return name.toASCIIString();
		}
	};

	/**
	 * A default implementation of {@link Reference#name()} that uses
	 * {@link #NOBODY}.
	 * 
	 * @implSpec the default implementation returns {@link NOBODY}'s
	 *           name
	 * @return the name of this actor
	 */
	@Override
	default URI name() {
		return NOBODY.name();
	}

	/**
	 * Provides the context of this actor. By default, it uses the
	 * context from {@link SystemContext} which is expected to be
	 * initialized in the beginning of an application using this API.
	 * 
	 * @see SystemContext
	 * @return the context to which this actor is registered with.
	 */
	default Context context() {
		return SystemContext.context();
	}

	/**
	 * Sends a general message to a recipient and captures the result
	 * into an instance of {@link Future}.
	 * 
	 * @see Context
	 * @see Router
	 * 
	 * @param <V>
	 *            the type of the result expected from the future value
	 * @param to
	 *            the receiver of the message
	 * @param message
	 *            the message to be sent to the receiver
	 * @return a future value to capture the result of processing the
	 *         message. The future value may throw exception is
	 *         {@link Future#get()} is used as a result of either
	 *         failure in processing the message or actually the
	 *         processing of the message decided to fail the message
	 *         result. The user of the future value may inspect into
	 *         causes of the exception to identify the reasons.
	 */
	default <V> Future<V> ask(Actor to, Object message) {
		final Actor receiver = NOBODY.equals(to) ? (Actor) reference(to) : to;
		final Envelope envelope = new SimpleEnvelope(self(), receiver, message);
		ForkJoinPool.commonPool().execute(() -> context().router().route(envelope));
		return envelope.response();
	}

	/**
	 * Sends a message to a reference.
	 *
	 * @param <V>
	 *            the type of the future value of the response of the
	 *            message
	 * @param to
	 *            the receiver of the message
	 * @param message
	 *            the message itself
	 * @return the future value to capture the result of the message
	 */
	default <V> Future<V> send(Reference to, Object message) {
		final Reference from = self();
		final Envelope envelope = new SimpleEnvelope(from, to, message);
		ForkJoinPool.commonPool().execute(() -> context().router().route(envelope));
		return envelope.response();
	}

	/**
	 * A convenient method to ask a method invocation from this actor
	 * reference. Delegates to {@link #invoke(Actor, String, Object...)}
	 * with {@code this} parameter.
	 * 
	 * @param <V>
	 *            the type of the result expected from the future value
	 * @param method
	 *            the name of the method to be invoked
	 * @param args
	 *            the actual parameters of the method
	 * @return a future value to capture the result (see
	 *         {@link #ask(Actor, Object)})
	 */
	default <V> Future<V> ask(String method, Object... args) {
		return invoke(this, method, args);
	}

	/**
	 * Sends a message to the recipient to invoke a specific method with
	 * specific arguments expected in the receiver object. Delegates to
	 * {@link #ask(Actor, Object)} with an instance of
	 * {@link MethodReference} by default.
	 * 
	 * @param <V>
	 *            the type of the result expected from the future value
	 * @param to
	 *            the receiver of the message to invoke the method
	 * @param method
	 *            the name of the method to be invoked
	 * @param args
	 *            the actual parameters of method to execute
	 * @return a future value to capture the result (see
	 *         {@link #ask(Actor, Object)})
	 */
	default <V> Future<V> invoke(Actor to, String method, Object... args) {
		final MethodReference message = MethodReference.of(to, method, args);
		return ask(to, message);
	}

	/**
	 * Provides access to the reference registered for this actor
	 * object.
	 * 
	 * @return the reference of this object
	 */
	default Reference self() {
		if (this instanceof ContextActor) {
			return this;
		}
		return reference(this);
	}

	/**
	 * Provides the sender of the <i>current</i> message that is being
	 * invoked/processed by the receiver object.
	 * 
	 * @see Context
	 * @see ContextActor
	 * @see EnvelopeContext
	 * 
	 * @return the sender of the current message or {@link #NOBODY} if
	 *         there is no sender for this message
	 */
	default Reference sender() {
		try {
			final Reference ref = self();
			if (ref instanceof ContextActor) {
				ContextActor caref = (ContextActor) ref;
				Context context = caref.context();
				if (context != null || context instanceof EnvelopeContext) {
					return ((EnvelopeContext) context).sender();
				}
			}
			if (!NOBODY.equals(this)) {
				Context context = context();
				if (context != null && context instanceof EnvelopeContext) {
					return ((EnvelopeContext) context).sender();
				}
			}
		} catch (Exception e) {
			// Ignore
		}
		return NOBODY;
	}
	
	/**
	 * Delegates to {@link Context#object(Reference)}.
	 * 
	 * @see Context#object(Reference)
	 * 
	 * @param <T>
	 *            the expected type of the object
	 * @param reference
	 *            the reference of the object
	 * @return See {@link Context#object(Reference)}
	 */
	default <T> T object(Reference reference) {
		return context().object(reference);
	}
	
	/**
	 * Delegates to {@link Context#reference(Object)}.
	 * 
	 * @see Context#reference(Object)
	 * 
	 * @param object
	 *            the object to find the reference for
	 * @return the reference of the object or {@code null}
	 */
	default Reference reference(Object object) {
		return context().reference(object);
	}

	/**
	 * The implementation is not different from
	 * {@link Reference#compareTo(Reference)}.
	 * 
	 * @param o
	 *            the reference to compare to
	 * @return the default semantics specified by {@link Comparable}
	 */
	@Override
	default int compareTo(Reference o) {
		return name().compareTo(o.name());
	}

}
