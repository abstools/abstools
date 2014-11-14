package abs.api;

import java.util.concurrent.CompletableFuture;

/**
 * An async inbox directly sends an envelope to opened using its
 * context's default opener using a
 * {@link java.util.concurrent.ForkJoinPool}. Note that this
 * implementation is not safe in the manner that there may be two
 * envelopes opened for the same recipient in the same time which is in
 * violation with ABS model; however, it may be useful for other
 * purposes such as testing.
 *
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class AsyncInbox extends AbstractInbox implements Inbox {

	/**
	 * <p>
	 * Constructor for AsyncInbox.
	 * </p>
	 */
	public AsyncInbox() {
	}

	/** {@inheritDoc} */
	@Override
	protected void open(final Opener opener, final Envelope envelope, final Object receiver) {
		CompletableFuture.<Object> supplyAsync(() -> {
			return opener.open(envelope, receiver);
		});
	}

}
