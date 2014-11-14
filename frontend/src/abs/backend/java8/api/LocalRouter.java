package abs.api;

/**
 * A local implementation of {@link abs.api.Router} that requires an
 * instance of {@link abs.api.Context} to work with. It tries to find
 * the proper {@link abs.api.Inbox} using the provided context.
 *
 * @see LocalNotary
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class LocalRouter implements Router {

	private Context context;

	/**
	 * Default ctor.
	 */
	public LocalRouter() {
	}

	/**
	 * <p>
	 * Constructor for LocalRouter.
	 * </p>
	 *
	 * @param context
	 *            a {@link abs.api.Context} object.
	 */
	public LocalRouter(Context context) {
		this.context = context;
	}

	/** {@inheritDoc} */
	@Override
	public void route(Envelope envelope) {
		final Reference to = envelope.to();
		final Object target = findTarget(to);
		final Inbox inbox = findInbox(envelope, target);
		// Note: target can be null. The reason is that the
		// envelope may actually carry a message that is not a
		// type that needs the target object. For instance, it can
		// be a Runnable or a Callable that can be executed
		// independent of the receiver of the message.
		inbox.post(envelope, target);
	}

	@Override
	public void bind(Context context) {
		this.context = context;
	}

	/**
	 * <p>
	 * findTarget.
	 * </p>
	 *
	 * @param to
	 *            a {@link abs.api.Reference} object.
	 * @return a {@link java.lang.Object} object.
	 */
	protected Object findTarget(Reference to) {
		Object object = context.notary().get(to);
		if (object != null) {
			return object;
		}
		// The actor itself can be a reference
		if (to instanceof Actor) {
			Reference actor = (Actor) to;
			if (actor.name().equals(to.name())) {
				return actor;
			}
		}
		return null;
	}

	/**
	 * <p>
	 * findInbox.
	 * </p>
	 *
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @param target
	 *            a {@link java.lang.Object} object.
	 * @return a {@link abs.api.Inbox} object.
	 */
	protected Inbox findInbox(Envelope envelope, Object target) {
		return context.inbox(envelope.to());
	}

}
