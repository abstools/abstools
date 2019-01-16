/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock;

import org.junit.Assume;

import deadlock.analyser.Analyser;
import abs.frontend.ast.Model;
import abs.frontend.typesystem.ExamplesTypeChecking;

public class DeadlockCheckerTests extends ExamplesTypeChecking {

    public DeadlockCheckerTests(String input, String product) {
        super(input, product);
    }

    @Override
    protected void onError(String err) {
        Assume.assumeTrue(err, false);
    }

    @Override
    protected Model parse(String input) throws Exception {
        Model m = super.parse(input);
        Assume.assumeTrue(m.hasMainBlock());
        (new Analyser()).deadlockAnalysis(m, true, 2, 2, System.out);
        return m;
    }
}
