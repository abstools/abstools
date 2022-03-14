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
     * The contents of values of the Any type can not be examined in detail,
     * but comparison operations should be supported.
     **/
    @Test
    public void anyCompareInt() {
        assertEvalTrue(
                String.join(System.lineSeparator(),
                    "{",
                        "Any x = 42;",
                        "Bool testresult = x == 42 && 42 == x && x != 0 && 0 != x;",
                    "}"
                )
        );
    }

    @Test
    public void anyCompareAny() {
        assertEvalTrue(
                String.join(System.lineSeparator(),
                    "{",
                        "Any x = 42;",
                        "Any y = 42;",
                        "Any z = 0;",
                        "Bool testresult = x == y && y == x && x != z && z != x;",
                    "}"
                )
        );
    }

    @Test
    public void anyCompareInterface() {
        assertEvalTrue(
                String.join(System.lineSeparator(),
                    "interface Foo {}",
                    "class Bar(Int x) implements Foo {}",

                    "{",
                        "Foo x = new Bar(42);",
                        "Foo y = new Bar(0);",

                        "Any anyX = x;",
                        "Any anyY = y;",

                        "Bool testresult = ",
                               "x == x && y == y && x != y && y != x",
                            "&& anyX == anyX && anyY == anyY && anyX != anyY && anyY != anyX",
                            "&& anyX == x && x == anyX && anyX != y && y != anyX",
                        ";",
                    "}"
                )
        );
    }

    /**
     * Using Any, futures can be compared.
     */
    @Test
    public void compareFutures() {
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
