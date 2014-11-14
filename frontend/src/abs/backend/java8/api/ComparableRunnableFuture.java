package abs.api;

import java.util.concurrent.RunnableFuture;

/**
 * TODO A currently internal API to be documented
 *
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface ComparableRunnableFuture extends RunnableFuture<Object>,
		Comparable<ComparableRunnableFuture> {

}
