package abs.frontend.pardef;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.Decl;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import java.util.regex.Pattern;
import org.junit.Test;

public class ParFnAppTest extends PardefTest {

    private FnApp assertHasCall(Model model, String expectedName) {
        FnApp result = getCall(model, Pattern.compile(expectedName));
        String errorMessage =
            "No expanded function call with name " + expectedName + " created"
                + " (functions: " + getFunctions(model) + ")";
        assertNotNull(errorMessage, result);
        return result;
    }

    private Model testExpand(Model model, String... expectedNames) {
        model = expand(model);
        for (String expectedName : expectedNames) {
            assertHasCall(model, expandedName(expectedName));
        }
        return model;
    }

    @Test(expected = PardefModellingException.class)
    public void tooManyFuncArgs() {
        expand(parse(
            "apply(inc, inc)(0);",
            incFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooFewFuncArgs() {
        expand(parse(
            "apply()(0);",
            applyFunction()
        ));
    }

    @Test
    public void tooManyArgsForFuncParam() {
        Model m = parse(
            "apply(tooFew)(0);",
            applyFunction(),
            "def Int tooFew() = 0;"
        );
        m.expandPartialFunctions();
        SemanticConditionList conditions = m.typeCheck();
        assertTrue(conditions.containsErrors());
    }

    @Test
    public void tooFewArgsForFuncParam() {
        Model m = parse(
            "apply(tooMany)(0);",
            applyFunction(),
            "def Int tooMany(Int i, Int j) = 0;"
        );
        m.expandPartialFunctions();
        SemanticConditionList conditions = m.typeCheck();
        assertTrue(conditions.containsErrors());
    }

    @Test(expected = PardefModellingException.class)
    public void tooManyArgs() {
        expand(parse(
            "apply(inc)(0, 1);",
            incFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooFewArgs() {
        expand(parse(
            "apply(inc)();",
            incFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void callingUnknown() {
        expand(parse(
            "apply(inc)(0);",
            incFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void simpleCycle() {
        // test -> test2 -> test
        expand(parse(
            "test()();",
            "def Int test()() = test2()();",
            "def Int test2()() = test()();"
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void bigCycle() {
        // test -> test2 -> test3 -> test
        expand(parse(
            "test()();",
            "def Int test()() = test2()();",
            "def Int test2()() = test3()();",
            "def Int test3()() = test()();"
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void indirectCycle() {
        // test -> test2 -> test3 -> test2
        expand(parse(
            "test()();",
            "def Int test()() = test2()();",
            "def Int test2()() = test3()();",
            "def Int test3()() = test2()();"
        ));
    }

    @Test
    public void recursion() {
        testExpand(parse(
            "List<Rat> l = map(halve)(Nil);",
            halveFunction()
        ), "Map_ABS_StdLib_halve");
    }

    @Test
    public void recursionWithClosure() {
        Model m = expand(parse(
            "Int x = 0; Int y = 1; rec((Int i) => x, (Int j) => y)();",
            "def Int rec(f, g)() = rec();"
        ));
        FnApp call = assertHasCall(m, expandedName("Rec_%s_Anon\\d+"));
        assertEquals(2, call.getNumParam());
    }

    @Test
    public void simpleParametricCall() {
        testExpand(parse(
            "Rat r = apply(halve)(2);",
            halveFunction(),
            applyFunction()
        ), "Apply_%s_halve");
    }

    @Test
    public void deepParametricCall() {
        // List<A> instead of just A in simple case
        testExpand(parse(
            "List<Rat> l = map(halve)(Nil);",
            halveFunction()
        ), "Map_ABS_StdLib_halve");
    }

    @Test
    public void parametricCallInBody() {
        testExpand(parse(
            "callApply(halve)(1);",
            halveFunction(),
            applyFunction(),
            "def D callApply<C, D>(fn)(C c) = apply(fn)(c);"
        ), "Apply_%s_halve", "CallApply_%s_halve");
    }

    @Test
    public void parametricIdentity() {
        testExpand(parse(
            "Int i = apply(identity)(0);"
                + "Rat j = apply(identity)(1/2);"
                + "String s = apply(identity)(\"foo\");",
            "def T identity<T>(T t) = t;",
            applyFunction()
        ), "Apply_%s_identity");
    }

    @Test
    public void parametricFunctionReturnType() {
        testExpand(parse(
            "Int i = applyOnString(two)(\"foo\");",
            "def T applyOnString<T>(fn)(String s) = fn(s);",
            "def Int two(String s) = 2;"
        ));
    }

    @Test
    public void fieldUseInAnon() {
        testExpand(assertParseOkStdLib(
            "def Int produce(fn)() = fn();\n"
                + "class C {\n"
                + "Int i = 1;\n"
                + "Unit m() {\n"
                + "Int j = produce(() => i)();"
                + "}\n"
                + "}\n"
        ));
    }

    @Test
    public void deepFieldUseInAnon() {
        testExpand(assertParseOkStdLib(
            "def Int produce(fn)() = fn();\n"
                + "class C {\n"
                + "Int i = 1;\n"
                + "Unit m() {\n"
                + "Int j = produce(() => 1 + i)();"
                + "}\n"
                + "}\n"
        ));
    }

    @Test
    public void fieldUseInParametric() {
        testExpand(assertParseOkStdLib(
            "class Test() {\n"
                + "  Int i = 0;\n"
                + "  Unit init() {\n"
                + "    List<Int> test = map((String s) => i)(list[\"foo\"]);\n"
                + "  }\n"
                + "}"
        ));
    }

    @Test
    public void nestedCalls() {
        testExpand(parse(
            "Int i = apply(inc)(apply(dec)(0));",
            incFunction(),
            decFunction(),
            applyFunction()
        ), "Apply_%s_dec", "Apply_%s_inc");
    }

    @Test(expected = PardefModellingException.class)
    public void invalidCallToAlreadyExpanded() {
        expand(parse(
            "apply(inc)(0); apply(inc)(0, 42);",
            incFunction(),
            applyFunction()
        ));
    }

    @Test
    public void callWithAnonymousFunction() {
        testExpand(parse(
            "apply((Int i) => i)(0);",
            applyFunction()
        ), "Apply_%s_Anon\\d+");

        testExpand(parse(
            "apply((Int i) => i + 1)(0);",
            applyFunction()
        ), "Apply_%s_Anon\\d+");

        testExpand(parse(
            "apply((Int i) => inc(i))(0);",
            applyFunction(),
            incFunction()
        ), "Apply_%s_Anon\\d+");

        testExpand(parse(
            "Int i = test(() => 1)();",
            "def Int test(f)() = f();"
        ), "Test_%s_Anon\\d+");
    }

    @Test(expected = PardefModellingException.class)
    public void anonymousTooManyArgs() {
        expand(parse(
            "apply((Int i, Int j) => i)(0);",
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void anonymousTooFewArgs() {
        expand(parse(
            "apply(() => i)(0);",
            applyFunction()
        ));
    }

    @Test
    public void anonymousSimpleClosure() {
        testExpand(parse(
            "Int x = 0; apply((Int i) => i + x)(0);",
            applyFunction()
        ), "Apply_%s_Anon\\d+");
    }

    @Test
    public void anonymousNestedClosure() {
        testExpand(parse(
            "Int x = 0; apply((Int i) => apply((Int j) => j + x)(i))(0);",
            applyFunction()
        ), "Apply_%s_Anon\\d+");
    }

    @Test
    public void closureParamSameNameAsFunctionParam() {
        testExpand(parse(
            "Int x = 1; test((Int i) => x + i)(0);",
            "def Int test(f)(Int x_0) = f(x_0);"
        ), "Test_%s_Anon\\d+");
    }

    @Test
    public void multipleAnonsUsingSameClosureVar() {
        testExpand(parse(
            "Int x = 1; test(() => x, () => -x)();",
            "def Int test(f, g)() = f() + g();"
        ), "Test_%s_Anon\\d+");
    }

    @Test
    public void sameAnonTwiceTwoClosureParams() {
        Model m = testExpand(parse(
            "Int x = 1; test(() => x, () => x)();",
            "def Int test(f, g)() = f() + g();"
        ), "Test_%s_Anon\\d+");
        FunctionDecl function = getFunction(m, Pattern.compile(expandedName("Test_%s_Anon\\d+")));
        assertNotNull(function);
        assertEquals(2, function.getNumParam());
        assertEquals("X0", function.getParam(0).getName());
        assertEquals("X1", function.getParam(1).getName());
    }

    @Test
    public void sameAnonTwiceTwoExpansions() {
        Model m = testExpand(parse(
            "apply((Int i) => i)(1);"
                + "apply((Int i) => i)(1);",
            applyFunction()
        ));
        ModuleDecl module = m.lookupModule("UnitTest");
        int foundExpansions = 0;
        for (Decl decl : module.getDecls()) {
            if (decl instanceof FunctionDecl) {
                FunctionDecl fun = (FunctionDecl) decl;
                if (fun.getName().startsWith("Apply_")) {
                    ++foundExpansions;
                }
            }
        }
        assertEquals(2, foundExpansions);
    }

    @Test
    public void importExpansion() {
        // name import
        testExpand(assertParseOkStdLib(
            "import test from Pardef;"
                + incFunction()
                + "{ test(inc)(); }"
                + "module Pardef; export *;"
                + "def Int test(f)() = f(0);"
        ), "Test_Pardef_inc");

        // star import
        testExpand(assertParseOkStdLib(
            "import * from Pardef;"
                + incFunction()
                + "{ test(inc)(); }"
                + "module Pardef; export *;"
                + "def Int test(f)() = f(0);"
        ), "Test_Pardef_inc");

        // import function and pardef
        testExpand(assertParseOkStdLib(
            "import test from Pardef;"
                + "import inc from IncMod;"
                + "{ test(inc)(); }"
                + "module Pardef; export *; def Int test(f)() = f(0);"
                + "module IncMod; export *; " + incFunction()
        ), "Test_Pardef_inc");
    }

    @Test
    public void usageInTrait() {
        testExpand(assertParseOkStdLib(
            "module ParFnAppInTrait;\n"
                + "\n"
                + "trait T = {\n"
                + "    Int m(){\n"
                + "        List<Int> l = list[1, 2, 3];\n"
                + "        return foldl((Int i, Int j) => i + j)(l, 0);\n"
                + "   }\n"
                + "}\n"
                + "\n"
                + "interface C {\n"
                + "\tInt m();\n"
                + "}\n"
                + "\n"
                + "class C implements C {\n"
                + "    uses T;\n"
                + "}\n"
                + "\n"
                + "{\n"
                + "\tC c = new C();\n"
                + "\tInt i = c.m();\n"
                + "}\n"), "Foldl_ABS_StdLib_Anon\\d+");
    }
}
