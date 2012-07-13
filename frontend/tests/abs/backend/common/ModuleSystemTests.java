/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import abs.backend.BackendTestDriver;

@RunWith(Parameterized.class)
public class ModuleSystemTests extends SemanticTests {
    public ModuleSystemTests(BackendTestDriver d) {
        super(d);
    }

    @Test
    public void simpleModule() {
        assertEvalTrue("module A; export Foo, Bar; data Foo = Bar; module Test; import A.Foo, A.Bar;"
                + " { A.Foo f = A.Bar; Bool testresult = True; } ");
    }

    @Test
    public void duplicateNameInDifferntModules() {
        assertEvalTrue("module A; data Foo = Baz; module B; data Foo = Bar; { Foo f = Bar; Bool testresult = True; }");
    }

}
