package abs.api;

import java.time.Instant;
import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;

/**
 * TODO An internal API to be documented.
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
class EnvelopeFutureTask extends FutureTask<Object> implements ComparableRunnableFuture {

	protected final Instant created = Instant.now();
	private final Envelope envelope;

	/**
	 * <p>
	 * Constructor for EnvelopeFutureTask.
	 * </p>
	 *
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @param command
	 *            a {@link java.util.concurrent.Callable} object.
	 */
	public EnvelopeFutureTask(Envelope envelope, Callable<Object> command) {
		super(command);
		this.envelope = envelope;
	}

	/** {@inheritDoc} */
	@Override
	public int compareTo(ComparableRunnableFuture c) {
		if (c instanceof EnvelopeFutureTask == false) {
			return 0;
		}
		EnvelopeFutureTask o = (EnvelopeFutureTask) c;
		if (envelope.to().compareTo(o.envelope.to()) != 0) {
			return 0;
		}
		return Long.compare(envelope.sequence(), o.envelope.sequence());
	}

}
