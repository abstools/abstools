/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typesystem;

import static org.junit.Assert.assertEquals;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.ExpFunctionDef;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ParametricFunctionDecl;
import org.abs_models.frontend.ast.ReturnStmt;
import org.abs_models.frontend.ast.TypeParameterDecl;
import org.abs_models.frontend.typechecker.DataTypeType;
import org.abs_models.frontend.typechecker.KindedName;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.TypeParameter;
import org.abs_models.frontend.typechecker.UnionType;
import org.junit.Test;

public class AnyTypeTests extends FrontendTest {
    /**
     * Since comparison across types is not defined in most cases, we forbid the
     * declaration of variables, parameters, etc. of Any Type
     **/
    @Test
    public void invalidAnyTypeUses() throws Exception {
        // No Any variables
        assertTypeErrors(
                String.join(System.lineSeparator(),
                    "{",
                        "Any x = 42;",
                    "}"
                )
        );

        // No any constructor args
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "data AnyWrapper = AnyWrapper(Any x);",
                "",
                "{",
                    "AnyWrapper a = AnyWrapper(42);",
                    "println(`$x(a)$`);",
                "}"
            )
        );

        // No Any constructor args
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "interface BI {",
                    "Unit m();",
                "}",
                "",
                "class B(Any x) implements BI {",
                    "Unit m() {",
                        "println(`$x$`);",
                    "}",
                "}",
                "",
                "{ }"
            )
        );

        // No misuse of any through type aliases
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "type TotallyNotAny = Any;",
                "",
                "{",
                    "TotallyNotAny x = 42;",
                "}"
            )
        );

        // No Any as return type of functions
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "def Any method() = 42;",
                "",
                "{",
                    "println(`$method() == 42$`);",
                "}"
            )
        );

        // No Any as return type of partial functions
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "def Any apply(fn)(Int value) = fn(a);",
                "",
                "{ }"
            )
        );

        // No Any as return type of parametric partial functions
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "def Any apply<A>(fn)(A value) = fn(a);",
                "",
                "{ }"
            )
        );

        // No binding of Any to type parameters
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "data Wrapper<A> = Wrapper(A unwrap);",

                "{",
                    "Wrapper<Any> wi = Wrapper(1);",
                    "Wrapper<Any> ws = Wrapper(\"Hello\");",

                    "Bool x = unwrap(wi) == unwrap(ws);",
                    "println(toString(x));",
                "}"
            )
        );
    }

    /**
     * Use of Any in futures should be possible
     */
    public void anyFutures() throws Exception {
        // One should be able to declare Fut<Any> variables
        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",

                    "Unit run() {",
                        "Fut<Any> f = this!myMethod();",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // And to compare Fut<...> variables across types with any
        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",

                    "Unit run() {",
                        "Fut<Any> f = this!myMethod();",
                        "Fut<Int> g = this!myMethod();",

                        "Bool x = f == f && f != g;",
                        "println(toString(x));",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // It should not be possible to extract and downcast values in Fut<Any>
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",
                    
                    "Unit run() {",
                        "Fut<Any> f = this!myMethod();",
                        
                        "Int x = f.get;",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // It should also not be possible to extract and store Any values through get
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",
                    
                    "Unit run() {",
                        "Fut<Any> f = this!myMethod();",
                    
                        "Any x = f.get;",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // Neither should it be possible through await:
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",
                    
                    "Unit run() {",
                        "Any x = await this!myMethod();",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // Any as return type of methods should be ok, because it can not be extracted
        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Any myMethod() {",
                        "return 42;",
                    "}",

                    "Unit run() {",
                        "Fut<Any> f = this!myMethod();",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // Casting to Any should also work for interface types
        assertTypeOK(
            String.join(System.lineSeparator(),
                "interface Foo {}",
                "class Bar implements Foo { }",
                
                "class MyClass {",
                    "Foo makeFoo() {",
                        "return new Bar();",
                    "}",
                    
                    "Unit run() {",
                        "Fut<Any> f = this!makeFoo();",
                    "}",
                "}",
                
                "{",
                    "new MyClass();",
                "}"
            )
        );
    }
}
