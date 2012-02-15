/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;

public class OtherAnalysisTests extends FrontendTest {
    
    @Test
    public void countCOG() {
        Model m = assertParseOk("interface I { } class C { { I i = new cog C(); } Unit m() { I i = new cog C(); } } { I i; i = new cog C(); i = new C(); while (true) { i = new cog C(); }}");
        assertEquals(4, m.countNumberOfNewCogExpr());
    }
    
    @Test
    public void finalTest() {
        assertParse("interface I { } { [Final] I i; i = null; }", Config.TYPE_CHECK, Config.WITH_STD_LIB, Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void fullcopyTest() {
        Model m = assertParseOk("module M; class C {}", Config.WITH_STD_LIB);
        Model m2 = m.fullCopy();
        assertFalse(m.hasErrors());
        assertFalse(m2.hasErrors());
    }

    @Test
    public void fullcopyTest1() {
        Model m = assertParseOk("module M; class C {}", Config.WITH_STD_LIB);
        Model m2 = m.fullCopy();
        assertFalse(m.hasErrors());
        assertFalse(m2.hasErrors());
        assertTrue(m.typeCheck().isEmpty());
        assertTrue(m2.typeCheck().isEmpty());
    }

    @Test
    public void fullcopyTest2() {
        Model m = assertParseOk("module M; class C {}", Config.WITH_STD_LIB);
        assertFalse(m.hasErrors());
        assertTrue(m.typeCheck().toString(),m.typeCheck().isEmpty());
        Model m2 = m.fullCopy();
        assertFalse(m2.hasErrors());
        assertTrue(m2.typeCheck().toString(),m2.typeCheck().isEmpty());
    }
}
