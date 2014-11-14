package abs.api;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * An implementation of opener that is used by {@link QueueInbox}.
 * This implementation requires an instance of {@link ExecutorService}
 * that dispatches the envelopes to be executed when their turn is
 * resolved.
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
class QueueOpener extends DefaultOpener {

	private final BlockingQueue<Runnable> tasks;
	private final EnvelopeTaskExecutor taskExecutor;
	private final Runnable onEnvelopeTaskComplete;

	/**
	 * <p>
	 * Constructor for QueueOpener.
	 * </p>
	 *
	 * @param tasks
	 *            a {@link java.util.concurrent.BlockingQueue} object.
	 * @param executor
	 *            a {@link java.util.concurrent.ExecutorService}
	 *            object.
	 */
	public QueueOpener(BlockingQueue<Runnable> tasks, ExecutorService executor) {
		this.tasks = tasks;
		this.taskExecutor = new EnvelopeTaskExecutor(tasks, executor);
		this.onEnvelopeTaskComplete = () -> {
			taskExecutor.tryExecuteNext();
		};
	}

	/** {@inheritDoc} */
	@Override
	protected void executeEnvelopeTask(final Runnable task) {
		tasks.offer(task);
		executeNext();
	}

	/** {@inheritDoc} */
	@Override
	protected Runnable createEnvelopeTask(Envelope envelope, Object target) {
		final Runnable task = super.createEnvelopeTask(envelope, target);
		final Callable<Object> command = () -> {
			super.executeEnvelopeTask(task);
			getOnEnvelopeTaskComplete().run();
			return null;
		};
		return new EnvelopeFutureTask(envelope, command);
	}

	/**
	 * <p>
	 * executeNext.
	 * </p>
	 */
	protected void executeNext() {
		this.taskExecutor.executeNext();
	}

	/**
	 * <p>
	 * Getter for the field <code>onEnvelopeTaskComplete</code>.
	 * </p>
	 *
	 * @return a {@link java.lang.Runnable} object.
	 */
	protected Runnable getOnEnvelopeTaskComplete() {
		return onEnvelopeTaskComplete;
	}

	static class EnvelopeTaskExecutor {

		private final BlockingQueue<Runnable> tasks;
		private final ExecutorService executor;
		private final AtomicBoolean busy = new AtomicBoolean(false);

		public EnvelopeTaskExecutor(BlockingQueue<Runnable> tasks, ExecutorService executor) {
			this.tasks = tasks;
			this.executor = executor;
		}

		protected void tryExecuteNext() {
			completeCurrent();
			executeNext();
		}

		protected void completeCurrent() {
			if (!busy.compareAndSet(true, false)) {
				throw new IllegalStateException("Should have been busy!");
			}
		}

		protected void executeNext() {
			if (!busy.compareAndSet(false, true)) {
				return;
			}
			Runnable task = tasks.poll();
			if (task == null) {
				busy.compareAndSet(true, false);
				return;
			}
			executor.submit(task);
		}

	}

}
