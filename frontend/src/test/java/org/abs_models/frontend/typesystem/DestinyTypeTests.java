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

public class DestinyTypeTests extends FrontendTest {
    @Test
    public void invalidDestinyTypeUses() throws Exception {
        // It should not be possible to downcast Destiny to a future
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",

                    "Unit run() {",
                        "Destiny f = this!myMethod();",
                        "Fut<Int> fi = f;",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // It should not be possible to assign random types to Destiny
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "{",
                    "Destiny d = 42;",
                "}"
            )
        );

        // It should not be possible to assign random types to Destiny even if
        // wrapped as as a type parameter
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "{",
                    "List<Destiny> li = list[1];",
                "}"
            )
        );

        // It should not be possible to extract values from Destiny via a get
        // expression
        assertTypeErrors(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",
                    
                    "Unit run() {",
                        "Destiny f = this!myMethod();",
                        
                        "Int x = f.get;",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        assertTypeErrors(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",
                    
                    "Unit run() {",
                        "Destiny f = this!myMethod();",
                        
                        "Int x = 0;",
                        "x = f.get;",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        assertTypeErrors(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int x = 0;",

                    "Int myMethod() {",
                        "return 42;",
                    "}",
                    
                    "Unit run() {",
                        "Destiny f = this!myMethod();",
                        
                        "x = f.get;",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // Applying `.get` to `destiny` should give a deadlock warning.
        assertWarnings(
            String.join(System.lineSeparator(),
                "class C {",
                    "Unit run() {",
                        "destiny.get;",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );
    }

    /**
     * Use of Destiny with futures should be possible
     */
    public void destinyFutures() throws Exception {
        // One should be able to declare Destiny variables and assign futures to
        // them
        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",

                    "Unit run() {",
                        "Destiny f = this!myMethod();",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // And to compare Fut<...> variables across types with Destiny
        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",

                    "Unit run() {",
                        "Destiny f = this!myMethod();",
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

        // Destiny as return type of methods should be ok
        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Destiny myMethod() {",
                        "return destiny;",
                    "}",

                    "Unit run() {",
                        "Destiny f = this!myMethod();",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // Casting to Destiny should also work for futures carrying interface types
        assertTypeOK(
            String.join(System.lineSeparator(),
                "interface Foo {}",
                "class Bar implements Foo { }",
                
                "class MyClass {",
                    "Foo makeFoo() {",
                        "return new Bar();",
                    "}",
                    
                    "Unit run() {",
                        "Destiny f = this!makeFoo();",
                    "}",
                "}",
                
                "{",
                    "new MyClass();",
                "}"
            )
        );

        // Upcasting of wrapped future types to Destiny should be ok
        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",

                    "Unit run() {",
                        "List<Destiny> l = list[this!myMethod()];",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // While it is not ok to extract a value from a get expression on a
        // Destiny value, it should be ok to synchronize via get
        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "destiny.get;",
                        "return 42;",
                    "}",
                    
                    "Unit run() {",
                        "this!myMethod();",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );


        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",
                    
                    "Unit run() {",
                        "Destiny f = this!myMethod();",
                        
                        "f.get;",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        // Similarly, it should be ok to await on a Destiny value
        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Unit run() {",
                        "Destiny f = destiny;",
                        
                        "await f?",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );

        assertTypeOK(
            String.join(System.lineSeparator(),
                "class C {",
                    "Int myMethod() {",
                        "return 42;",
                    "}",
                    
                    "Unit run() {",
                        "Destiny f = this!myMethod();",
                        
                        "await f?",
                    "}",
                "}",

                "{",
                    "new C();",
                "}"
            )
        );
    }
}
