package abs.api;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.Future;

/**
 * An implementation of opener that resolves the message in the
 * following order
 * <ul>
 * <li>if the message is an instance of {@link java.lang.Runnable}
 * <li>if the message is an instance of
 * {@link java.util.concurrent.Callable}
 * <li>if the message is a method invocation encapsulated by an
 * instance of {@link abs.api.MethodReference}
 * <li>if the recipient of the message is an instance of
 * {@link abs.api.Behavior} then opens the envelope by running the
 * messages inside the recipient object.
 * </ul>
 *
 * @see QueueOpener
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class DefaultOpener implements Opener {

	private final ConcurrentMap<MethodReference, Method> methodCache = new ConcurrentSkipListMap<>(
			MethodReference.COMPARATOR);
	private final Map<Method, MethodHandle> methodHandleCache = new LinkedHashMap<>(1024,
			0.75f, true);

	/** {@inheritDoc} */
	@Override
	public <V> Future<V> open(final Envelope envelope, final Object target) {
		return execute(envelope, target);
	}

	/**
	 * <p>
	 * execute.
	 * </p>
	 *
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @param target
	 *            a {@link java.lang.Object} object.
	 * @param <V>
	 *            a V object.
	 * @return a {@link java.util.concurrent.Future} object.
	 */
	protected <V> Future<V> execute(final Envelope envelope, final Object target) {
		final Fut future = envelope.response();
		Runnable task = createEnvelopeTask(envelope, target);
		if (task == null) {
			future.completeExceptionally(new IllegalArgumentException("Invalid message: "
					+ envelope.message()));
		} else {
			try {
				executeEnvelopeTask(task);
			} catch (Exception e) {
				future.completeExceptionally(e);
			}
		}
		return (Future<V>) future;
	}

	/**
	 * <p>
	 * executeEnvelopeTask.
	 * </p>
	 *
	 * @param task
	 *            a {@link java.lang.Runnable} object.
	 */
	protected void executeEnvelopeTask(Runnable task) {
		task.run();
	}

	/**
	 * <p>
	 * createEnvelopeTask.
	 * </p>
	 *
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @param target
	 *            a {@link java.lang.Object} object.
	 * @return a {@link java.lang.Runnable} object.
	 */
	protected Runnable createEnvelopeTask(final Envelope envelope, final Object target) {
		final Object msg = envelope.message();
		if (target instanceof Behavior) {
			return fromActorEnvelope(envelope, (Behavior) target);
		} else if (msg instanceof MethodReference) {
			return fromMethodReferenceEnvelope(envelope, target);
		} else if (msg instanceof Runnable) {
			return fromRunnableEnvelope(envelope);
		} else if (msg instanceof Callable) {
			return fromCallableEnvelope(envelope);
		}
		return null;
	}

	/**
	 * <p>
	 * fromCallableEnvelope.
	 * </p>
	 *
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @return a {@link java.lang.Runnable} object.
	 */
	protected Runnable fromCallableEnvelope(final Envelope envelope) {
		return () -> {
			final Fut future = envelope.response();
			try {
				final Object result = ((Callable<?>) envelope.message()).call();
				future.complete(result);
			} catch (Exception e) {
				future.completeExceptionally(e);
			}
		};
	}

	/**
	 * <p>
	 * fromRunnableEnvelope.
	 * </p>
	 *
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @return a {@link java.lang.Runnable} object.
	 */
	protected Runnable fromRunnableEnvelope(final Envelope envelope) {
		return () -> {
			final Fut future = envelope.response();
			try {
				((Runnable) envelope.message()).run();
				future.complete(null);
			} catch (Exception e) {
				future.completeExceptionally(e);
			}
		};
	}

	/**
	 * <p>
	 * fromMethodReferenceEnvelope.
	 * </p>
	 *
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @param target
	 *            a {@link java.lang.Object} object.
	 * @return a {@link java.lang.Runnable} object.
	 */
	protected Runnable fromMethodReferenceEnvelope(final Envelope envelope, final Object target) {
		return () -> {
			final Fut future = envelope.response();
			try {
				MethodReference method = (MethodReference) envelope.message();
				if (target == null) {
					// A method reference should be executed up a
					// non-null object reference.
					future.completeExceptionally(new RuntimeException(
							"No object can be found with reference: " + method.owner()));
					return;
				}
				MethodHandle handle = createMethodHandle(method, target);
				handle = handle.bindTo(target);
				Object result = handle.invokeWithArguments(method.args());
				future.complete(result);
			} catch (Throwable e) {
				future.completeExceptionally(e);
			}
		};
	}

	/**
	 * <p>
	 * fromActorEnvelope.
	 * </p>
	 *
	 * @param envelope
	 *            a {@link abs.api.Envelope} object.
	 * @param target
	 *            a {@link abs.api.Behavior} object.
	 * @return a {@link java.lang.Runnable} object.
	 */
	protected Runnable fromActorEnvelope(final Envelope envelope, final Behavior target) {
		return () -> {
			final Fut future = envelope.response();
			try {
				Object result = target.respond(envelope.message());
				future.complete(result);
			} catch (Exception e) {
				future.completeExceptionally(e);
			}
		};

	}

	/**
	 * <p>
	 * createReflectionMethod.
	 * </p>
	 *
	 * @param msg
	 *            a {@link abs.api.MethodReference} object.
	 * @param target
	 *            a {@link java.lang.Object} object.
	 * @return a {@link java.lang.reflect.Method} object.
	 * @throws java.lang.NoSuchMethodException
	 *             if any.
	 */
	protected Method createReflectionMethod(final MethodReference msg, final Object target)
			throws NoSuchMethodException {
		final Object[] args = msg.args();
		final Class<?>[] types;
		if (args != null) {
			types = new Class<?>[args.length];
			for (int i = 0; i < types.length; ++i) {
				types[i] = args[i].getClass();
			}
		} else {
			types = null;
		}
		final Class<?> targetClass = target.getClass();
		final Method reflectionMethod = targetClass.getMethod(msg.name().toString(), types);
		return reflectionMethod;
	}

	/**
	 * <p>
	 * createMethodHandle.
	 * </p>
	 *
	 * @param msg
	 *            a {@link abs.api.MethodReference} object.
	 * @param target
	 *            a {@link java.lang.Object} object.
	 * @return a {@link java.lang.invoke.MethodHandle} object.
	 * @throws java.lang.NoSuchMethodException
	 *             if any.
	 * @throws java.lang.IllegalAccessException
	 *             if any.
	 */
	protected MethodHandle createMethodHandle(final MethodReference msg, final Object target)
			throws NoSuchMethodException, IllegalAccessException {
		Method method = methodCache.get(msg);
		if (method == null) {
			method = createReflectionMethod(msg, target);
			methodCache.putIfAbsent(msg, method);
		}
		MethodHandle methodHandle = methodHandleCache.get(method);
		if (methodHandle == null) {
			methodHandle = MethodHandles.lookup().unreflect(method);
			methodHandleCache.putIfAbsent(method, methodHandle);
		}
		return methodHandle;
	}

}
