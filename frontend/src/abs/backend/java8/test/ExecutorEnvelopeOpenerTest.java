package abs.api;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.PriorityBlockingQueue;

import org.junit.Test;

/**
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class ExecutorEnvelopeOpenerTest {

	private final List<Long> actualSequence = new ArrayList<>();
	private final List<Long> expectedSequence = new ArrayList<>();

	private class SequencingExecutorEnvelopeOpener extends QueueOpener {
		private final Sequencer sequencer = Sequencer.of(0L);

		private SequencingExecutorEnvelopeOpener(BlockingQueue<Runnable> tasks,
				ExecutorService executor, Sequencer sequencer) {
			super(tasks, executor);
		}

		@Override
		protected Runnable getOnEnvelopeTaskComplete() {
			Runnable first = super.getOnEnvelopeTaskComplete();
			return () -> {
				first.run();
				actualSequence.add(sequencer.get());
			};
		}
	}

	@Test
	public void taskExecutionOrderShouldBeSequential() throws Exception {
		Context context = new LocalContext();
		ExecutorService executor = new OrderedThreadPoolExecutor(new PriorityBlockingQueue<>());
		BlockingQueue<Runnable> tasks = new PriorityBlockingQueue<>();
		Sequencer sequencer = Sequencer.of(0L);
		QueueOpener opener = new SequencingExecutorEnvelopeOpener(tasks, executor,
				sequencer);
		Reference sender = context.newActor("s", new Object());
		Reference receiver = context.newActor("r", new Object());
		MethodReference method = MethodReference.of(receiver, "hashCode");

		for (int i = 1; i <= 5; ++i) {
			final long seq = i;
			Envelope envelope = new SimpleEnvelope(sender, receiver, method);
			final Runnable envelopeTask = opener.createEnvelopeTask(envelope, receiver);
			tasks.add(envelopeTask);
			expectedSequence.add(seq);
		}
		opener.executeNext();

		Thread.sleep(100);
		assertEquals(expectedSequence, actualSequence);
	}

}
