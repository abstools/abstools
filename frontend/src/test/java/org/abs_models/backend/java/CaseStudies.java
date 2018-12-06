/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import static org.abs_models.backend.java.JavaBackendTest.*;

import org.abs_models.frontend.typesystem.CaseStudyTypeChecking;
import org.junit.Test;

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
