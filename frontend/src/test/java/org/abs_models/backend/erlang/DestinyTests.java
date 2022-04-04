package org.abs_models.backend.erlang;

import org.abs_models.ABSTest;
import org.junit.Test;

public class DestinyTests extends ABSTest {
    private final ErlangTestDriver driver = new ErlangTestDriver();

    /**
     * Main use of destiny is comparing PIDs
     */
    @Test
    public void destinyComparisonTests() throws Exception {
        driver.assertEvalTrue(
                String.join(System.lineSeparator(),
                    "module MainMod;",

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

                        "Destiny futureStore1 = aCall;",
                        "Destiny futureStore2 = bCall;",
                        "Destiny futureStore3 = aCall;",

                        "Bool testresult = aCall == futureStore1 && bCall != futureStore1 && futureStore1 != futureStore2 && futureStore1 == futureStore3;",
                    "}"
                )
        );

        // Destiny is the same as the future received by the caller
        driver.assertEvalTrue(
            String.join(System.lineSeparator(),
                "module Main;",

                "interface Testable {",
                    "Bool test();",
                "}",

                "class MyClass implements Testable {",
                    "Fut<Int> f;",
                    "Destiny g;",
                    
                    "Bool result = False;",
                    
                    "Int m() {",
                        "g = destiny;",
                        
                        "await this!writeResult();",
                        
                        "return 42;",
                    "}",
                    
                    "Unit writeResult() {",
                        "result = g == f;",
                    "}",
                    
                    "Bool test() {",
                        "this.f = this!m();",
                        
                        "await this.f?;",
                        
                        "return result;",
                    "}",
                "}",

                "{",
                    "Testable t = new MyClass();",
                    
                    "Bool testresult = await t!test();",
                "}"
            )
        );

        // ...this should also hold true when the futures have already completed
        driver.assertEvalTrue(
            String.join(System.lineSeparator(),
                "module Main;",

                "interface Testable {",
                    "Bool test();",
                "}",

                "class MyClass implements Testable {",
                    "Fut<Int> f;",
                    "Destiny g;",
                    
                    "Int m() {",
                        "g = destiny;",
                        
                        "return 42;",
                    "}",
                    
                    "Bool test() {",
                        "this.f = this!m();",
                        
                        "await this.f?;",
                        
                        "return this.f == this.g;",
                    "}",
                "}",

                "{",
                    "Testable t = new MyClass();",
                    
                    "Bool testresult = await t!test();",
                "}"
            )
        );

        // Futures of different calls should also be different
        driver.assertEvalTrue(
            String.join(System.lineSeparator(),
                "module Main;",

                "interface Testable {",
                    "Bool test();",
                "}",

                "class MyClass implements Testable {",
                    "Maybe<Destiny> leftDestiny = Nothing;",
                    "Maybe<Destiny> rightDestiny = Nothing;",
                    
                    "Fut<Int> left;",
                    "Fut<Int> right;",
                    
                    "Bool done = False;",
                    "Bool result = False;",
                    
                    "Int m(Bool writeLeft) {",
                        "if (writeLeft) {",
                            "leftDestiny = Just(destiny);",
                        "}",
                        
                        "else {",
                            "rightDestiny = Just(destiny);",
                        "}",
                        
                        "await done;",
                        
                        "return 42;",
                    "}",
                    
                    "Bool test() {",
                        "this.left = this!m(True);",
                        "this.right = this!m(False);",
                        
                        "await isJust(leftDestiny) && isJust(rightDestiny);",
                        
                        "result =",
                            "fromJust(this.leftDestiny) == this.left &&",
                            "fromJust(this.rightDestiny) == this.right &&",
                            "this.left != this.right &&",
                            "fromJust(this.leftDestiny) != fromJust(this.rightDestiny);",
                        
                        "done = True;",
                        
                        "return result;",
                    "}",
                "}",

                "{",
                    "Testable t = new MyClass();",
                    
                    "Bool testresult = await t!test();",
                "}"
            )
        );

        // We can synchronize via a get expression
        driver.assertEvalTrue(
            String.join(System.lineSeparator(),
                "module MainMod;",

                "interface Testable {",
                    "Bool test();",
                "}",

                "class A implements Testable {",
                    "Bool test() {",
                        "return True;",
                    "}",
                "}",

                "{",
                    "Testable t = new A();",
                    "Destiny f = t!test();",

                    "f.get;",

                    "Bool testresult = True;",
                "}"
            )
        );
    }
}


