/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import java.io.File;

import org.abs_models.backend.BackendTestDriver;
import org.junit.Assume;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class AnyTests extends SemanticTests {
    public AnyTests(BackendTestDriver d) {
        super(d);
    }

    /**
     * Using Any, futures can be compared.
     */
    @Test
    public void compareFutures() throws Exception {
        assertEvalTrue(
                String.join(System.lineSeparator(),
                    "interface Foo {",
                        "Unit a();",
                        "Int b();",
                    "}",

                    "class Bar implements Foo {",
                        "Unit a() {}",
                        "Int b() { return 42; }",
                    "}",

                    "{",
                        "Foo o = new Bar();",

                        "Fut<Unit> aCall = o!a();",
                        "Fut<Int> bCall = o!b();",

                        "Fut<Any> futureStore1 = aCall;",
                        "Fut<Any> futureStore2 = bCall;",
                        "Fut<Any> futureStore3 = aCall;",

                        "Bool testresult = aCall == futureStore1 && bCall != futureStore1 && futureStore1 != futureStore2 && futureStore1 == futureStore3;",
                    "}"
                )
        );
    }
}
