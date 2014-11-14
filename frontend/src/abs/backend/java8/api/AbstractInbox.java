package abs.api;

import java.util.concurrent.Future;

/**
 * An abstract implementation of inbox that uses an instance of
 * {@link Context} to resolve a proper {@link Opener} for each envelope
 * and use the opener to open the envelope.
 * 
 * @see Inbox
 * @see DispatchInbox
 * @see AsyncInbox
 * @see QueueInbox
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class AbstractInbox implements Inbox {

	protected Context context;

	/**
	 * <p>
	 * Constructor for AbstractInbox.
	 * </p>
	 */
	public AbstractInbox() {
	}

	/** {@inheritDoc} */
	@Override
	public <V> Future<V> post(final Envelope envelope, final Object receiver) {
		final Opener opener = opener(envelope, receiver);
		onOpen(envelope, opener, receiver);
		open(opener, envelope, receiver);
		return envelope.response();
	}

	/** {@inheritDoc} */
	@Override
	public void bind(Context context) {
		this.context = context;
	}

	/**
	 * <p>
	 * onOpen.
	 * </p>
	 *
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @param opener
	 *            a {@link abs.api.Opener} object.
	 * @param receiver
	 *            a {@link java.lang.Object} object.
	 */
	protected void onOpen(Envelope envelope, Opener opener, Object receiver) {
		synchronized (envelope) {
			Actor ref = (Actor) envelope.to();
			if (ref instanceof ContextActor) {
				ContextActor cref = (ContextActor) ref;
				cref.bind(EnvelopeContext.of(envelope, context));
			}
		}
	}

	/**
	 * <p>
	 * open.
	 * </p>
	 *
	 * @param opener
	 *            a {@link abs.api.Opener} object.
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @param receiver
	 *            a {@link java.lang.Object} object.
	 */
	protected void open(final Opener opener, final Envelope envelope, final Object receiver) {
		opener.open(envelope, receiver);
	}

	/**
	 * <p>
	 * opener.
	 * </p>
	 *
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @param receiver
	 *            a {@link java.lang.Object} object.
	 * @return a {@link abs.api.Opener} object.
	 */
	protected Opener opener(Envelope envelope, Object receiver) {
		return context.opener(envelope.to());
	}

}
