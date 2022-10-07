package org.abs_models.backend.erlang;

import java.io.File;

import org.abs_models.ABSTest;
import org.junit.Test;

public class DestinyTests extends ABSTest {
    private final ErlangTestDriver driver = new ErlangTestDriver();

    /**
     * Main use of destiny is comparing PIDs
     */
    @Test
    public void destinyComparisonTests() throws Exception {
        driver.assertEvalTrue(
            new File("abssamples/backend/DestinyTests/destiny_simple_value_comparison.abs")
        );

        // Destiny is the same as the future received by the caller
        driver.assertEvalTrue(
            new File("abssamples/backend/DestinyTests/call_future_is_same_as_destiny.abs")
        );

        // ...this should also hold true when the futures have already completed
        driver.assertEvalTrue(
            new File("abssamples/backend/DestinyTests/comparing_completed_destinies.abs")
        );

        // Futures of different calls should also be different
        driver.assertEvalTrue(
            new File("abssamples/backend/DestinyTests/destiny_simple_value_comparison2.abs")
        );

        // We can synchronize via a get expression
        driver.assertEvalTrue(
            new File("abssamples/backend/DestinyTests/get_synchronization_on_destiny.abs")
        );

        // A future is generated even when it is not assigned to a variable
        // and we can synchronize on the destiny of the call
        driver.assertEvalTrue(
            new File("abssamples/backend/DestinyTests/destiny_for_unassigned_calls.abs")
        );

        // destiny in a synchronous call resolves to the destiny of the
        // task who made the call
        driver.assertEvalTrue(
            new File("abssamples/backend/DestinyTests/destiny_of_synchronous_call.abs")
        );

        // destiny resolves to the same future that is returned by the original 
        // asynchronous call
        driver.assertEvalTrue(
            new File("abssamples/backend/DestinyTests/call_future_is_same_as_destiny2.abs")
        );
    }

    @Test
    public void destinyOfRunMethodIsValid() throws Exception {
        // https://github.com/abstools/abstools/issues/313
        // destiny was null inside a run method
        driver.assertEvalTrue(
            new File("abssamples/backend/DestinyTests/destiny_of_run_is_not_null.abs"));
    }
}
