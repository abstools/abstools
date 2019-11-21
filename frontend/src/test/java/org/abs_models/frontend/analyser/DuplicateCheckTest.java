/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.analyser;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.Model;

public class DuplicateCheckTest extends FrontendTest {

    @Test
    public void interfaceDuplicates() {
        Model p = assertParse("interface I {} interface I {}");
        assertContains(p.typeCheck().getFirstError(), ErrorMessage.DUPLICATE_TYPE_DECL.withArgs("I", ""));
    }

    @Test
    public void datatypeDuplicates() {
        Model p = assertParse("data D; data D;");
        assertContains(p.typeCheck().getFirstError(), ErrorMessage.DUPLICATE_TYPE_DECL.withArgs("D", ""));
    }

    @Test
    public void typeDuplicates() {
        Model p = assertParse("data D; interface D { }");
        assertContains(p.typeCheck().getFirstError(), ErrorMessage.DUPLICATE_TYPE_DECL.withArgs("D", ""));
    }

    @Test
    public void typeDuplicates2() {
        Model p = assertParse("data D; data X; type D = X;");
        assertContains(p.typeCheck().getFirstError(), ErrorMessage.DUPLICATE_TYPE_DECL.withArgs("D", ""));
    }

    @Test
    public void duplicateClasses() {
        Model p = assertParse("class C {} class C {}");
        assertContains(p.typeCheck().getFirstError(), ErrorMessage.DUPLICATE_CLASS_NAME.withArgs("C", ""));
    }

    @Test
    public void duplicateFunctions() {
        Model p = assertParse("data Test = Test; def Test zero() = Test; def Test zero() = Test;");
        assertContains(p.typeCheck().getFirstError(), ErrorMessage.DUPLICATE_FUN_NAME.withArgs("zero", ""));
    }

    @Test
    public void duplicatePartialFunctions() {
        Model m = assertParse("data Test = Test; def Test zero()() = Test; def Test zero()() = Test;");
        assertContains(m.typeCheck().getFirstError(), ErrorMessage.DUPLICATE_PARTIAL_FUN_NAME.withArgs("zero", ""));
    }

    private void assertContains(SemanticCondition expected, String actual) {
        assertNotNull("Duplicate not detected", expected);
        assertTrue("Expected that " + expected.getHelpMessage() + " contains " + actual,
                   // remove trailing ‘.’ from actual message
                   expected.getHelpMessage().contains(actual.substring(0, actual.length() - 1)));
    }

}
