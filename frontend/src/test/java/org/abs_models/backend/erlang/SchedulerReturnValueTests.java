package org.abs_models.backend.erlang;

import java.io.File;

import org.abs_models.ABSTest;
import org.junit.Test;

public class SchedulerReturnValueTests extends ABSTest {
    private final ErlangTestDriver driver = new ErlangTestDriver();

    @Test
    public void plainReturnTypeWorks() throws Exception {
        driver.assertEvalTrue(
            new File("abssamples/backend/TimeTests/scheduler_plain_return_value.abs")
        );
    }

    @Test
    public void maybeReturnTypeWorks() throws Exception {
        driver.assertEvalTrue(
            new File("abssamples/backend/TimeTests/scheduler_just_return_value.abs")
        );
    }

    @Test
    public void complexScheduler() throws Exception {
        // This scheduler ensures that a resource is accessed in the correct order: connect -> perform action -> disconnect
        driver.assertEvalTrue(
            new File("abssamples/backend/TimeTests/scheduler_complex_return_conditions.abs")
        );
    }
}



