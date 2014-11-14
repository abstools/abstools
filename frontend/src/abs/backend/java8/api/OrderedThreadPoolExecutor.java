package abs.api;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * TODO An internal API to be documented
 *
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class OrderedThreadPoolExecutor extends ThreadPoolExecutor {

	private static final int NCPU = Runtime.getRuntime().availableProcessors();

	/**
	 * <p>
	 * Constructor for OrderedThreadPoolExecutor.
	 * </p>
	 *
	 * @param queue
	 *            a {@link java.util.concurrent.BlockingQueue} object.
	 */
	public OrderedThreadPoolExecutor(final BlockingQueue<Runnable> queue) {
		super(1, NCPU, 1, TimeUnit.MINUTES, queue, new RejectedExecutionHandler() {
			@Override
			public void rejectedExecution(Runnable r, ThreadPoolExecutor executor) {
				// ignore
			}
		});
	}

	/** {@inheritDoc} */
	@Override
	protected <T> RunnableFuture<T> newTaskFor(Callable<T> callable) {
		if (callable instanceof ComparableRunnableFuture == false) {
			throw new IllegalArgumentException("Task is not an instance of "
					+ ComparableRunnableFuture.class + " : " + callable);
		}
		return (RunnableFuture<T>) callable;
	}

	/** {@inheritDoc} */
	@Override
	protected <T> RunnableFuture<T> newTaskFor(Runnable runnable, T value) {
		if (runnable instanceof ComparableRunnableFuture == false) {
			throw new IllegalArgumentException("Task is not an instance of "
					+ ComparableRunnableFuture.class + " : " + runnable);
		}
		return (RunnableFuture<T>) runnable;
	}

}
