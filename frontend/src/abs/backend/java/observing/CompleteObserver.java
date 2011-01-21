package abs.backend.java.observing;

/**
 * A convenience interface that represents all possible observers
 * 
 * @author Jan Schäfer
 *
 */
public interface CompleteObserver extends SystemObserver, ObjectCreationObserver, ObjectObserver, TaskSchedulerObserver {
}
