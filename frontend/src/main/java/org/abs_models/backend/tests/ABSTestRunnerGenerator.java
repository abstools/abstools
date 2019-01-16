/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.tests;

import java.io.PrintStream;

import org.abs_models.frontend.ast.Model;

/**
 * 
 * @author pwong
 *
 */
public interface ABSTestRunnerGenerator {

    static final String RUNNER_MAIN = "AbsUnit.TestRunner";
    
    /**
     * Outputs an ABS module that only contains a main block. For each test
     * method defined in the model, this main blo annotated test classes
     * 
     * @param stream
     */
    void generateTestRunner(PrintStream stream);
    
    /**
     * Checks if this generator contains a {@link Model} that defines ABSUnit
     * tests.
     * 
     * @return
     */
    boolean hasUnitTest();
    
}
