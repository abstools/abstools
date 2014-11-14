package abs.api;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.junit.Before;
import org.junit.Test;

/**
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class DefaultEnvelopeOpenerTest {

	private Context context;

	@Before
	public void setUp() {
		this.context = new LocalContext();
	}

	@Test
	public void testExecuteMethodReference() throws Exception {
		final Object object = new Object();
		final Integer result = object.hashCode();
		Reference from = context.newActor("a", new Object());
		Reference to = context.newActor("b", object);
		MethodReference message = MethodReference.of(to, "hashCode");
		Envelope e = new SimpleEnvelope(from, to, message);
		DefaultOpener p = new DefaultOpener();
		Future<?> f = p.open(e, object);
		assertNotNull(f);
		assertEquals(e.response(), f);
		assertTrue(f.isDone());
		assertEquals(result, f.get());
	}

	@Test
	public void testExecuteRunnable() throws Exception {
		Reference from = context.newActor("a", new Object());
		Reference to = context.newActor("b", new Object());
		Runnable message = to::name;
		Envelope e = new SimpleEnvelope(from, to, message);
		Opener p = new DefaultOpener();
		Future<?> f = p.open(e, to);
		assertNotNull(f);
		assertEquals(f, e.response());
		assertTrue(f.isDone());
		assertNull(f.get());
	}

	@Test
	public void testExecuteCallable() throws Exception {
		Reference from = context.newActor("a", new Object());
		Reference to = context.newActor("b", new Object());
		Callable<?> message = to::name;
		Envelope e = new SimpleEnvelope(from, to, message);
		Opener p = new DefaultOpener();
		Future<?> f = p.open(e, to);
		assertNotNull(f);
		assertEquals(f, e.response());
		assertTrue(f.isDone());
		assertEquals(to.name(), f.get());
	}

	@Test
	public void testExecuteBehavior() throws Exception {
		Reference from = context.newActor("a", new Object());
		Actor to = context.newActor("b", new Object());
		BehaviorActor toActor = new BehaviorActor(to);
		String message = UUID.randomUUID().toString();
		Envelope e = new SimpleEnvelope(from, toActor, message);
		Opener p = new DefaultOpener();
		Future<?> f = p.open(e, toActor);
		assertNotNull(f);
		assertEquals(f, e.response());
		assertTrue(f.isDone());
		assertEquals(message, f.get());
	}

	@Test(expected = ExecutionException.class)
	public void testExecuteWithException() throws Exception {
		Reference from = context.newActor("a", new Object());
		Reference to = context.newActor("b", new Object());
		final RuntimeException x = new RuntimeException("x");
		Runnable message = () -> {
			throw x;
		};
		Envelope e = new SimpleEnvelope(from, to, message);
		Opener p = new DefaultOpener();
		Future<?> f = p.open(e, to);
		assertNotNull(f);
		assertEquals(f, e.response());
		assertTrue(f.isDone());
		f.get();
	}

	private class BehaviorActor implements Behavior, Actor {

		private static final long serialVersionUID = 1L;

		private final Actor ref;

		public BehaviorActor(Actor ref) {
			this.ref = ref;
		}

		@Override
		public Object respond(Object message) {
			return message;
		}

		@Override
		public URI name() {
			return ref.name();
		}

		@Override
		public Context context() {
			return context;
		}

	}

}
