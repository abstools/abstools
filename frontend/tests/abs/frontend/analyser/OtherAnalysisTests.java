/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Exp;
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
    

}
