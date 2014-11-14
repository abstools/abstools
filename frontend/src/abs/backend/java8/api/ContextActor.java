package abs.api;

import java.net.URI;

/**
 * An extension of actor reference that allows an instance of
 * {@link Actor} to bind to a specific {@link Context}.
 * 
 * @see Context
 * @see Contextual
 * @see EnvelopeContext
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
interface ContextActor extends Actor, Contextual {

	/**
	 * Creates an instance with the specific name and the provided
	 * context. No validation or checks are performed by this method in
	 * the creation.
	 *
	 * @param name
	 *            the name of the actor
	 * @param context
	 *            the initial bound context
	 * @return an instance of {@link abs.api.ContextActor}
	 */
	static ContextActor of(Reference name, Context context) {
		return new LocalContextActor(name, context);
	}

	/**
	 * An implementation of {@link ContextActor} that is suitable for a
	 * local context.
	 */
	static class LocalContextActor implements ContextActor {

		private static final long serialVersionUID = 5903306592703776997L;

		private transient Context context;
		private final Reference name;

		public LocalContextActor(Reference name, Context context) {
			this.name = name;
			this.context = context;
		}

		@Override
		public final URI name() {
			return name.name();
		}

		@Override
		public Context context() {
			return context;
		}

		@Override
		public final void bind(Context context) {
			this.context = context;
		}

		@Override
		public int hashCode() {
			return name().hashCode();
		}

		@Override
		public String toString() {
			return name().toASCIIString();
		}

	}

}
