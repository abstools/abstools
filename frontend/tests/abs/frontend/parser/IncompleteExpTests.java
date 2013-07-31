/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import org.junit.Test;

import abs.frontend.FrontendTest;
import static abs.ABSTest.Config.*;

public class IncompleteExpTests extends FrontendTest {
    
    @Test
    public void incompleteSyncAccess() {
        assertParseOk("{ x = x.; }", ALLOW_INCOMPLETE_EXPR); 
        assertParseOk("{ x.; }", ALLOW_INCOMPLETE_EXPR);
    }

    @Test
    public void incompleteAsyncAccess() {
        assertParseOk("{ x = x!; }", ALLOW_INCOMPLETE_EXPR); 
        assertParseOk("{ x!; }", ALLOW_INCOMPLETE_EXPR); 
    }
    
    @Test
    public void incompleteThisAsyncAccess() {
        assertParse(" { this!x await x!foo();}", ALLOW_INCOMPLETE_EXPR, EXPECT_PARSE_ERROR); 
    }

    @Test
    public void incompleteNewExp() {
        assertParseOk("{ new ; }", ALLOW_INCOMPLETE_EXPR); 
        assertParseOk("{ new cog ; }", ALLOW_INCOMPLETE_EXPR); 
        assertParseOk("{ x = new ; }", ALLOW_INCOMPLETE_EXPR); 
        assertParseOk("{ x = new cog ; }", ALLOW_INCOMPLETE_EXPR); 
    }
    
    @Test
    public void incompleteStmt() {
        assertParseOk("class C { Unit m() { I i; i.; } }", ALLOW_INCOMPLETE_EXPR); 
    }
}
