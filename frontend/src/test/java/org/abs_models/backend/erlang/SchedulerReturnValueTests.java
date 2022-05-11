package org.abs_models.backend.erlang;

import org.abs_models.ABSTest;
import org.junit.Test;

public class SchedulerReturnValueTests extends ABSTest {
    private final ErlangTestDriver driver = new ErlangTestDriver();

    @Test
    public void plainReturnTypeWorks() throws Exception {
        driver.assertEvalTrue(
                String.join(System.lineSeparator(),
                    "module MainMod;",
                    "import * from ABS.Scheduler;",
                    
                    "def Process plainScheduler(List<Process> queue) = head(queue);",
                    
                    "interface Testable {",
                      "Bool test();",
                    "}",
                    
                    "[Scheduler: plainScheduler(queue)]",
                    "class C implements Testable {",
                      "Int counter = 0;",
                    
                      "Unit inc() {",
                        "counter = counter + 1;",
                      "}",
                    
                      "Bool test() {",
                        "this!inc();",
                        "this!inc();",
                        "this!inc();",
                        "await counter == 3;",
                    
                        "return True;",
                      "}",
                    "}",
                    
                    "{",
                      "Testable t = new C();",
                      "Bool testresult = await t!test();",
                    "}"
                )
        );
    }

    @Test
    public void maybeReturnTypeWorks() throws Exception {
        driver.assertEvalTrue(
                String.join(System.lineSeparator(),
                    "module MainMod;",
                    "import * from ABS.Scheduler;",
                    
                    "def Maybe<Process> maybeScheduler(List<Process> queue) = Just(head(queue));",
                    
                    "interface Testable {",
                      "Bool test();",
                    "}",
                    
                    "[Scheduler: maybeScheduler(queue)]",
                    "class C implements Testable {",
                      "Int counter = 0;",
                    
                      "Unit inc() {",
                        "counter = counter + 1;",
                      "}",
                    
                      "Bool test() {",
                        "this!inc();",
                        "this!inc();",
                        "this!inc();",
                        "await counter == 3;",
                    
                        "return True;",
                      "}",
                    "}",
                    
                    "{",
                      "Testable t = new C();",
                      "Bool testresult = await t!test();",
                    "}"
                )
        );
    }

    @Test
    public void complexScheduler() throws Exception {
        // This scheduler ensures that a resource is accessed in the correct order: connect -> perform action -> disconnect
        driver.assertEvalTrue(
                String.join(System.lineSeparator(),
                    "module MainMod;",
                    "import * from ABS.Scheduler;",
                    
                    "def Maybe<Process> resourceScheduler(List<Process> queue, Object connection) =",
                      "when method(head(queue)) == \".init\" || method(head(queue)) == \"isProtocolOk\" then Just(head(queue)) else",
                        "let List<Process> permittedActivations = filter(",
                            "(Process p) => ",
                              "when (connection == null) then",
                                "method(p) == \"connect\"",
                              "else",
                                "method(p) == \"performAction\" || method(p) == \"disconnect\" ",
                          ")(queue)",
                        "in",
                          "case permittedActivations {",
                            "Nil => Nothing |",
                            "Cons(p, _) => Just(",
                              "head(permittedActivations)",
                            ")",
                          "};",
                    
                    "interface SharedResource {",
                      "Unit connect();",
                      "Unit performAction();",
                      "Unit disconnect();",
                    
                      "Bool isProtocolOk();",
                    "}",
                    
                    "class Connection {}",
                    
                    "[Scheduler: resourceScheduler(queue, connection)]",
                    "class SharedResourceImpl implements SharedResource {",
                      "Object connection;",
                    
                      "Int correctOrder = 0;",
                    
                      "Unit connect() {",
                        "if (connection == null) {",
                          "correctOrder = 1;",
                        "}",
                    
                        "connection = new Connection();",
                        "println(\"connect\");",
                      "}",
                      "Unit performAction()  {",
                        "if (connection != null && correctOrder == 1) {",
                          "correctOrder = 2;",
                        "}",
                    
                        "println(\"performAction\");",
                      "}",
                    
                      "Unit disconnect() {",
                        "if (connection != null && correctOrder == 2) {",
                          "correctOrder = 3;",
                        "}",
                    
                        "connection = null;",
                        "println(\"disconnect\");",
                      "}",
                    
                      "Bool isProtocolOk() {",
                        "return correctOrder == 3;",
                      "}",
                    "}",
                    
                    "interface Testing {",
                      "Bool test();",
                    "}",
                    
                    "class Tester implements Testing {",
                      "Bool test() {",
                        "SharedResource r = new SharedResourceImpl();",
                    
                        "Fut<Unit> action = r!performAction();",
                        "await duration(5);",
                        "r!connect();",
                    
                        "await action?;",
                        "await r!disconnect();",
                    
                        "Bool ok = await r!isProtocolOk();",
                    
                        "return ok;",
                      "}",
                    "}",
                    
                    "{",
                      "Testing t = new Tester();",
                      "Bool testresult = await t!test();",
                    "}"
                )
        );
    }
}



