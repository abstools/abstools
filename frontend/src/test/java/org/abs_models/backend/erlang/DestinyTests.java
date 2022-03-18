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
        // Destiny is the same as the future received by the caller
        driver.assertEvalTrue(
            String.join(System.lineSeparator(),
                "module Main;",

                "interface Testable {",
                    "Bool test();",
                "}",

                "class MyClass implements Testable {",
                    "Fut<Int> f;",
                    "Fut<Any> g;",
                    
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
                    "Fut<Any> g;",
                    
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
                    "Maybe<Fut<Any>> leftDestiny = Nothing;",
                    "Maybe<Fut<Any>> rightDestiny = Nothing;",
                    
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
    }
}


