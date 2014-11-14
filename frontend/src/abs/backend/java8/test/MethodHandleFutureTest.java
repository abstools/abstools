package abs.api;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

/**
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class MethodHandleFutureTest {

	private static final int NCPU = Runtime.getRuntime().availableProcessors();
	private static final ExecutorService sharedExecutor = new ThreadPoolExecutor(1, NCPU, 1,
			TimeUnit.MINUTES, new SynchronousQueue<>());
	private static final TargetMethodHandleExecutor targetExecutor = new TargetMethodHandleExecutor();

	static class Target {
		private static final AtomicLong seq = new AtomicLong(0);

		public long exec() throws InterruptedException, ExecutionException {
			long sequence = seq.incrementAndGet();
			sharedExecutor.submit(new AsyncDependency(sequence)).get();
			return sequence;
		}
	}

	static class AsyncDependency implements Callable<Long> {
		private final long id;

		public AsyncDependency(long sequence) {
			this.id = sequence;
		}

		public Long call() throws InterruptedException {
			Thread.sleep(1000);
			targetExecutor.execByMethodHandle(new Target());
			return id;
		}
	}

	static class TargetMethodHandleExecutor {

		public Object execByMethodHandle(Target target) {
			try {
				Method method = Target.class.getMethod("exec");
				MethodHandle mh = MethodHandles.lookup().unreflect(method);
				mh = mh.bindTo(target);
				return mh.invokeWithArguments();
			} catch (Throwable e) {
				e.printStackTrace();
				return null;
			}
		}

	}

	public MethodHandleFutureTest() {
		final TargetMethodHandleExecutor executor = new TargetMethodHandleExecutor();
		for (int i = 0; i < 10; ++i) {
			sharedExecutor.submit(() -> {
				executor.execByMethodHandle(new Target());
			});
		}
	}

	public static void main(String[] args) {
		new MethodHandleFutureTest();
	}

}
