package abs.api;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.PriorityBlockingQueue;

/**
 * The queue inbox implements the concept of concurrent object (COG)
 * group messages queue from ABS. Every envelope is sent to the queue
 * and is processed based on the ordering that is implemented by the
 * queue executor.
 *
 * @see QueueOpener
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class QueueInbox extends AbstractInbox {

	private final QueueOpener opener;

	/**
	 * <p>
	 * Constructor for QueueInbox.
	 * </p>
	 *
	 * @param executor
	 *            a {@link java.util.concurrent.ExecutorService} object.
	 */
	public QueueInbox(ExecutorService executor) {
		this.opener = new QueueOpener(new PriorityBlockingQueue<>(4096), executor);
	}

	/** {@inheritDoc} */
	@Override
	protected Opener opener(final Envelope envelope, final Object receiver) {
		return opener;
	}

}
