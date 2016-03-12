/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typesystem;

import org.junit.Assert;

import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.Model;
import abs.frontend.parser.ParseSamplesTest;

public class ExamplesTypeChecking extends ParseSamplesTest {

    public ExamplesTypeChecking(String input, String product) {
        super(input,product);
    }

    /**
     * May be refined to Assume.assumeTrue(err,false) when
     * reusing this code for other test runners.
     */
    protected void onError(String err) {
        Assert.fail(err);
    }

    @Override
    protected Model parse(String input) throws Exception {
        Model m = assertTypeCheckFileOk(input, true);
        if (product != null) {
            m.flattenForProduct(product);
            final SemanticConditionList errors = m.getErrors();
            if (errors.containsErrors())
                onError(errors.getFirstError().getMessage());
            m.typeCheck(errors);
            if (errors.containsErrors())
                onError(errors.getFirstError().getMessage());
        }
        return m;
    }
}
