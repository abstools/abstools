/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typesystem;

import org.junit.Assert;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.ParseSamplesTest;

public class ExamplesTypeChecking extends ParseSamplesTest {

    public ExamplesTypeChecking(String input, String product) {
        super(input,product);
    }

    @Override
    protected Model parse(String input) throws Exception {
        Model m = assertTypeCheckFileOk(input, true);
        if (product != null) {
            m.flattenForProduct(product);
            final SemanticErrorList errors = m.getErrors();
            if (!errors.isEmpty())
                Assert.fail(errors.getFirst().getMessage());
            m.typeCheck(errors);
            if (!errors.isEmpty())
                Assert.fail(errors.getFirst().getMessage());
        }
        return m;
    }
}
