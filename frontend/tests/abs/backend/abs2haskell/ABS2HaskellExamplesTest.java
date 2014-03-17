/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.abs2haskell;

import org.junit.Assume;
import org.junit.BeforeClass;
import org.junit.Test;

import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.List;
import abs.frontend.typesystem.ExamplesTypeChecking;

/**
 * @author stolz
 */
public class ABS2HaskellExamplesTest extends ExamplesTypeChecking {

    public ABS2HaskellExamplesTest(String input, String product) {
        super(input, product);
    }

    @BeforeClass
    public static void checkRequired() {
        Assume.assumeTrue(ABS2HaskellDriver.checkA2HS());
    }

    @Override
    protected void onError(String err) {
        Assume.assumeTrue(err, false);
    }

    @Test
    @Override
    public void test() throws Exception {
        super.test();
        if (m.hasProductLine()) {
            /* Remember that the constructor flattened the file already!
             * Now we explicitly disconnect any PLs,
             * because we can't parse them. Arguably they shouldn't
             * be part of the resulting model. TODO?
             */
            for (int i = 0; i < m.getNumCompilationUnit(); i++) {
                CompilationUnit u = m.getCompilationUnit(i);
                if (u.hasProductLine()) {
                    u.getProductLineOpt().removeChild(0);
                    assert !u.hasProductLine();
                }
                if (u.hasDeltaDecl()) {
                    List<DeltaDecl> l = new List<DeltaDecl>();
                    u.setDeltaDeclList(l);
                    assert !u.hasDeltaDecl();
                }
                u.flushCache();
            }
            m.flushCache();
        }
        new ABS2HaskellDriver().generateAndCompile(m);
    }
}
