package abs.api;

import java.util.concurrent.Future;

/**
 * An envelope opener should allow a recipient object process an
 * incoming message from another object and finalize the result of the
 * message. The message should be opened in a {@link abs.api.Context}
 * where the object may possibly need to access other information such
 * as the sender of the message.
 *
 * @see Context
 * @see Inbox
 * @see Actor#sender()
 * @author Behrooz Nobakht
 * @since 1.0
 */
@FunctionalInterface
public interface Opener {

	/**
	 * Opens the envelope, runs a process inside the context of the
	 * target object to prepare the result of the envelope that is
	 * captured at the time the messages was sent. It is highly
	 * recommended that the implementation does not throw an exception
	 * and instead capture an fault into the future result of the
	 * envelope.
	 *
	 * @param envelope
	 *            the envelope to be opened
	 * @param target
	 *            the recipient object that binds to the reference by
	 *            {@link abs.api.Envelope#to()}
	 * @return the eventual result of the message which is already
	 *         captured by another actor reference and should complete
	 *         or fail the future of the {@link abs.api.Envelope}.
	 *         Note that the returned here may not actually be used as
	 *         it is already used by using
	 *         {@link java.util.concurrent.Future#get()}.
	 * @param <V>
	 *            a V object.
	 */
	<V> Future<V> open(Envelope envelope, Object target);

}
