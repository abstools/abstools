package org.abs_models.frontend.typesystem;

import org.abs_models.frontend.FrontendTest;
import org.junit.Test;

public class DestinyTypeTests extends FrontendTest {
    @Test
    public void destinyTyping() throws Exception {
        // Destiny is of Fut<Any> type and can not be assigned to a more
        // concrete type
        assertTypeErrors(
                String.join(System.lineSeparator(),
                    "class MyClass {",
                        "Int m() {",
                            "Fut<Int> x = destiny;",
                            
                            "return 42;",
                        "}",
                    "}",

                    "{",
                    "}"
                )
        );

        // Conversely, assigning to Fut<Any> works
        assertTypeOK(
                String.join(System.lineSeparator(),
                    "class MyClass {",
                        "Int m() {",
                            "Fut<Any> x = destiny;",
                            
                            "return 42;",
                        "}",
                    "}",

                    "{",
                    "}"
                )
        );

        // destinyOf returns a Fut<Any>
        assertTypeOK(
                String.join(System.lineSeparator(),
                    "module M;",
                    "import * from ABS.Scheduler;",

                    "def Process typecheck(Fut<Any> f, Process p) = p;",

                    "def Process scheduler(List<Process> queue) =",
                        "let Process p = head(queue) in typecheck(destinyOf(p), p);",

                    "[Scheduler: scheduler(queue)]",
                    "class MyClass { }",

                    "{",
                    "}"
                )
        );

        assertTypeErrors(
                String.join(System.lineSeparator(),
                    "import * from ABS.Scheduler;",

                    "def Process typecheck(Fut<Int> f, Process p) = p;",

                    "def Process scheduler(List<Process> queue) =",
                        "let Process p = head(queue) in typecheck(destinyOf(p), p);",

                    "[Scheduler: scheduler(queue)]",
                    "class MyClass { }",

                    "{",
                    "}"
                )
        );
    }
}
