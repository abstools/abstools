/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import static abs.frontend.analyser.ErrorMessage.CYCLIC_INHERITANCE;
import static abs.frontend.analyser.ErrorMessage.UNKOWN_INTERFACE;
import static org.junit.Assert.*;

import java.util.Iterator;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;

public class InterfaceDeclarationTest extends FrontendTest {

    @Test
    public void trivial() {
        Model p = assertParseOk("interface I {} {}");
        assertTrue(!p.getErrors().containsErrors());
    }

    @Test
    public void extending() {
        Model p = assertParseOk("interface I {} interface J extends I {} {}");
        assertTrue(!p.getErrors().containsErrors());
    }

    @Test
    public void extendingReversed() {
        Model p = assertParseOk("interface J extends I {} interface I {} {}");
        assertTrue(!p.getErrors().containsErrors());
    }

    @Test
    public void extendingUndefined() {
        Model p = assertParseOk("interface J extends I {} {}");
        assertEquals(1,p.getErrors().getErrorCount());
        assertEndsWith(p.getErrors().getFirstError(), UNKOWN_INTERFACE.withArgs("I"));
    }

    @Test
    public void circular() {
        Model p = assertParseOk("interface I extends I {} {}");
        assertEquals(1,p.getErrors().getErrorCount());
        assertEndsWith(p.getErrors().getFirstError(), CYCLIC_INHERITANCE.withArgs("I"));
    }

    @Test
    public void mutuallyCircular() {
        Model p = assertParseOk("interface I extends J {} interface J extends I {} {}");
        assertEquals(2,p.getErrors().getErrorCount());
        Iterator<SemanticCondition> i = p.getErrors().iterator();
        assertEndsWith(i.next(), CYCLIC_INHERITANCE.withArgs("I"));
        assertEndsWith(i.next(), CYCLIC_INHERITANCE.withArgs("J"));
    }

    @Test
    public void mutuallyCircularIndirect() {
        Model p = assertParseOk("interface I extends J {}  interface J extends K {}  interface K extends I {}");
        assertEquals(3,p.getErrors().getErrorCount());
        Iterator<SemanticCondition> i = p.getErrors().iterator();
        assertEndsWith(i.next(), CYCLIC_INHERITANCE.withArgs("I"));
        assertEndsWith(i.next(), CYCLIC_INHERITANCE.withArgs("J"));
        assertEndsWith(i.next(), CYCLIC_INHERITANCE.withArgs("K"));
    }

    private void assertEndsWith(SemanticCondition expected, String actual) {
        assertTrue("Expected that " + expected.getHelpMessage() + " ends with " + actual, expected.getHelpMessage()
                .endsWith(actual));
    }

}
