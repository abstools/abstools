package abs.api;

/**
 * An envelope context provides a snapshot view of a {@link Context}
 * based on a single bound {@link Envelope} instance. The main purpose
 * is to create a design pattern that facilitates provisioning of a
 * sender actor reference in the period an envelope is being opened by
 * its recipient. An envelope context is always bound to one instance of
 * envelope and one instance of context.
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface EnvelopeContext extends Context {

	/**
	 * The envelope of this context.
	 * 
	 * @return the envelope bound to this context
	 */
	Envelope envelope();

	/**
	 * The sender of an envelope context naturally is the sender of the
	 * bound envelope.
	 * 
	 * @return the sender of the envelope of this context
	 */
	default Reference sender() {
		return envelope().from();
	}

	/**
	 * Creates an instance of {@link EnvelopeContext} with the envelope
	 * and context provided.
	 * 
	 * @param envelope
	 *            the envelope of the new context
	 * @param context
	 *            the bound context
	 * @return an instance of {@link EnvelopeContext}
	 */
	static EnvelopeContext of(Envelope envelope, Context context) {
		return new SimpleEnvelopeContext(envelope, context);
	}

	/**
	 * An implementation of {@link EnvelopeContext} that delegates its
	 * behavior to the provided {@link Context} instance.
	 */
	static class SimpleEnvelopeContext implements EnvelopeContext {

		private final Envelope envelope;
		private final Context context;

		public SimpleEnvelopeContext(Envelope envelope, Context context) {
			this.envelope = envelope;
			this.context = context;
		}

		@Override
		public Envelope envelope() {
			return envelope;
		}

		public Actor newActor(String name, Object object) {
			return context.newActor(name, object);
		}

		public Router router() {
			return context.router();
		}

		public Notary notary() {
			return context.notary();
		}

		public Inbox inbox(Reference reference) {
			return context.inbox(reference);
		}

		public Opener opener(Reference reference) {
			return context.opener(reference);
		}

	}

}
