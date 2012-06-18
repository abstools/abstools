/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import static abs.backend.java.JavaBackendTest.*;

import org.junit.Test;
import abs.frontend.typesystem.CaseStudyTypeChecking;

public class CaseStudies extends CaseStudyTypeChecking {

    public CaseStudies(String input) {
        super(input);
    }

    @Test @Override
    public void test() throws Exception {
        super.test();
        assertValidJava(getJavaCode(m));
    }

}
