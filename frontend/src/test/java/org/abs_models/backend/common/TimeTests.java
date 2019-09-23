/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import java.io.File;

import org.abs_models.backend.BackendTestDriver;
import org.junit.Assume;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class TimeTests extends SemanticTests {
    public TimeTests(BackendTestDriver d) {
        super(d);
    }

    @Test
    public void ticket258() {
        assertEvalTrue("{ Bool testresult = True; await duration(1,1); }");
    }

    @Test
    public void duration1() {
        assertEvalTrue("{ Bool testresult = True; duration(1,1); }");
    }

    @Test
    public void costStmtBug160() {
        assertEvalTrue(new File("abssamples/backend/TimeTests/bug160.abs"));
    }

    @Test
    public void deadline1() throws Exception {
        Assume.assumeTrue("Only meaningful with Timed ABS support", driver.supportsTimedAbs());
        assertEvalTrue(new File("abssamples/backend/TimeTests/deadline1.abs"));
    }

    @Test
    public void scheduler_deadline() {
        Assume.assumeTrue("Only meaningful with Timed ABS and custom scheduler support",
                          driver.supportsTimedAbs() && driver.supportsCustomSchedulers());
        assertEvalTrue(new File("abssamples/backend/TimeTests/scheduler_deadline.abs"));
    }

    @Test
    public void scheduler_fields() {
        Assume.assumeTrue("Only meaningful with Timed ABS and custom scheduler support",
                          driver.supportsTimedAbs() && driver.supportsCustomSchedulers());
        assertEvalTrue(new File("abssamples/backend/TimeTests/scheduler_fields.abs"));
    }

    @Test
    public void resource_accounting() {
        Assume.assumeTrue("Only meaningful with Timed ABS support", driver.supportsTimedAbs());
        assertEvalTrue(new File("abssamples/backend/TimeTests/resource_accounting.abs"));
    }

    @Test
    public void no_time_advance1() {
        Assume.assumeTrue("Only meaningful with Timed ABS support", driver.supportsTimedAbs());
        assertEvalTrue(new File("abssamples/backend/TimeTests/no_time_advance1.abs"));
    }
}
