package abs.api;

import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

/**
 * A sequencer is a thread-safe unique {@link java.lang.Long}
 * generator.
 *
 * @author Behrooz Nobakht
 * @since 1.0
 */
@FunctionalInterface
public interface Sequencer extends Supplier<Long> {

	/**
	 * Creates a sequencer with starting from a specific value.
	 *
	 * @param start
	 *            the starting point for the sequencer
	 * @return an instance of {@link abs.api.Sequencer}
	 */
	static Sequencer of(Long start) {
		return new AtomicSequencer(start);
	}

	/**
	 * A sequencer using {@link AtomicLong} as its thread-safe
	 * concurrent storage.
	 */
	static class AtomicSequencer implements Sequencer {

		private final AtomicLong sequence;

		public AtomicSequencer(long start) {
			sequence = new AtomicLong(start);
		}

		@Override
		public Long get() {
			return sequence.incrementAndGet();
		}
	}

}
