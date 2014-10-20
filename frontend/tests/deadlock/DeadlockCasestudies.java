/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock;

import org.junit.Assume;
import org.junit.Test;

import deadlock.analyser.Analyser;
import abs.frontend.typesystem.CaseStudyTypeChecking;

public class DeadlockCasestudies extends CaseStudyTypeChecking {

    public DeadlockCasestudies(String input) {
        super(input);
    }

    @Test @Override
    public void test() throws Exception {
        super.test();
        Assume.assumeTrue("Needs main block",m.hasMainBlock());
       (new Analyser()).deadlockAnalysis(m, true, 2, 2, System.out);
    }
}
