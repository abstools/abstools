package abs.api;

/**
 * A life cycle supports phases of execution in the run time of the
 * environment. This is a general high-level interface to allow
 * different abstractions support the same behavior from top-down
 * design approach.
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface Lifecycle {

	/**
	 * Initializes the implementing life cycle.
	 * 
	 * @throws Exception
	 *             if the implementation decides to interrupt or
	 *             escalate the initialization error
	 */
	default void initialize() throws Exception {
	}

	/**
	 * Starting a life cycle ensures that the implementation is ready
	 * to be used after the completion of this method.
	 * 
	 * @throws Exception
	 *             if the implementation decides to interrupt or
	 *             escalate the problem
	 */
	default void start() throws Exception {
	}

	/**
	 * After a life cycle is stopped, the implementing object cannot
	 * be used.
	 * 
	 * @throws Exception
	 *             if stopping has problem and needs to be escalated
	 */
	default void stop() throws Exception {
	}

}
