package abs.api;

import java.util.concurrent.Future;

/**
 * An inbox allows a message to be posted to it for its recipient. The
 * implementation of {@link #post(Envelope, Object)} can vary from
 * directly opening the envelope or delegating to an instance
 * {@link abs.api.Opener}.
 *
 * @see Opener
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface Inbox extends Contextual {

	/**
	 * Posts an envelope to the inbox for the receiver object and
	 * captures a future value as the result. Note that the result of
	 * this method may or may not use
	 * {@link abs.api.Envelope#response()}. Moreover, note that it is
	 * highly recommended that this method does not throw an exception
	 * and instead capture any execution problem into the future result.
	 *
	 * @param envelope
	 *            the envelope to be posted
	 * @param receiver
	 *            the recipient of the envelope
	 * @return a future value capturing the result of eventually opening
	 *         the envelop by the receiver
	 * @param <V>
	 *            a V object.
	 */
	<V> Future<V> post(Envelope envelope, Object receiver);

}
