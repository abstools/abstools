/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typesystem;

import abs.frontend.ast.Model;
import abs.frontend.parser.ParseSamplesTest;

public class ExamplesTypeChecking extends ParseSamplesTest {

    public ExamplesTypeChecking(String input) {
        super(input);
    }

    @Override
    protected Model parse(String input) throws Exception {
        return assertTypeCheckFileOk(input, true);
    }
}
