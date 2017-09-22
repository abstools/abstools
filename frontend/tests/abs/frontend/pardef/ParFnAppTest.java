package abs.frontend.pardef;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import abs.frontend.ast.FnApp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import java.util.regex.Pattern;
import org.junit.Test;

public class ParFnAppTest extends PardefTest {

    private FnApp assertHasCall(Model model, String expectedName) {
        FnApp result = getCall(model, Pattern.compile(expectedName));
        String errorMessage = "No expanded function call with name " + expectedName + " created"
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
            "apply<Int, Int>(inc, inc)(0);",
            incFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooFewFuncArgs() {
        expand(parse(
            "apply<Int, Int>()(0);",
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooManyArgs() {
        expand(parse(
            "apply<Int, Int>(inc)(0, 1);",
            incFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooFewArgs() {
        expand(parse(
            "apply<Int, Int>(inc)();",
            incFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void callingUnknown() {
        expand(parse(
            "apply<Int, Int>(inc)(0);",
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
            "List<Rat> l = map<Int, Rat>(halve)(Nil);",
            halveFunction(),
            "def List<B> map<A, B>(f)(List<A> list) = case list {\n"
                + "Cons(x, xs) => Cons(f(x), map(xs));\n"
                + "Nil => Nil;\n"
                + "};"
        ), "Map_%s_halve_Int_Rat");
    }

    @Test
    public void recursionWithClosure() {
        Model m = expand(parse(
            "Int x = 0; Int y = 1; rec((Int i) => x, (Int j) => y)();",
            "def Int rec(f, g)() = rec();"
        ));
        FnApp call = assertHasCall(m, expandedName("Rec_%s_\\d+_\\d+"));
        assertEquals(2, call.getNumParam());
    }

    @Test
    public void parametricCall() {
        testExpand(parse(
            "Rat r = apply<Int, Rat>(halve)(2);",
            halveFunction(),
            applyFunction()
        ), "Apply_%s_halve_Int_Rat");
        testExpand(parse(
            "List<Rat> l = map<Int, Rat>(halve)(Nil);",
            halveFunction(),
            "def List<B> map<A, B>(f)(List<A> list) = case list {\n"
                + "Cons(x, xs) => Cons(f(x), map(xs));\n"
                + "Nil => Nil;\n"
                + "};"
        ), "Map_%s_halve_Int_Rat");
    }

    @Test(expected = PardefModellingException.class)
    public void parametricCallWithoutParamsNotPossible() {
        expand(parse(
            "apply(halve)(2);",
            halveFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooFewTypeParams() {
        expand(parse(
            "apply<Int>(halve)(2);",
            halveFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooManyTypeParams() {
        expand(parse(
            "apply<Int, Rat, Int>(halve)(2);",
            halveFunction(),
            applyFunction()
        ));
    }

    @Test
    public void nestedCalls() {
        testExpand(parse(
            "Int i = apply<Int, Int>(inc)(apply<Int, Int>(dec)(0));",
            incFunction(),
            decFunction(),
            applyFunction()
        ), "Apply_%s_dec_Int_Int", "Apply_%s_inc_Int_Int");
    }

    @Test(expected = PardefModellingException.class)
    public void invalidCallToAlreadyExpanded() {
        expand(parse(
            "apply<Int, Int>(inc)(0); apply<Int, Int>(inc)(0, 42);",
            incFunction(),
            applyFunction()
        ));
    }

    @Test
    public void callWithAnonymousFunction() {
        testExpand(parse(
            "apply<Int, Int>((Int i) => i)(0);",
            applyFunction()
        ));

        testExpand(parse(
            "apply<Int, Int>((Int i) => i + 1)(0);",
            applyFunction()
        ));

        testExpand(parse(
            "apply<Int, Int>((Int i) => inc(i))(0);",
            applyFunction(),
            incFunction()
        ), "Apply_%s_\\d+_Int_Int");

        testExpand(parse(
            "Int i = test(() => 1)();",
            "def Int test(f)() = f();"
        ), "Test_%s_\\d+");
    }

    @Test(expected = PardefModellingException.class)
    public void anonymousTooManyArgs() {
        expand(parse(
            "apply<Int, Int>((Int i, Int j) => i)(0);",
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void anonymousTooFewArgs() {
        expand(parse(
            "apply<Int, Int>(() => i)(0);",
            applyFunction()
        ));
    }

    @Test
    public void anonymousSimpleClosure() {
        testExpand(parse(
            "Int x = 0; apply<Int, Int>((Int i) => i + x)(0);",
            applyFunction()
        ), "Apply_%s_\\d+_Int_Int");
    }

    @Test
    public void anonymousNestedClosure() {
        testExpand(parse(
            "Int x = 0; apply<Int, Int>((Int i) => apply<Int, Int>((Int j) => j + x)(i))(0);",
            applyFunction()
        ), "Apply_%s_\\d+_Int_Int");
    }

    @Test
    public void closureParamSameNameAsFunctionParam() {
        testExpand(parse(
            "Int x = 1; test((Int i) => x + i)(0);",
            "def Int test(f)(Int x_0) = f(x_0);"
        ), "Test_%s_\\d+");
    }

    @Test
    public void multipleAnonsUsingSameClosureVar() {
        testExpand(parse(
            "Int x = 1; test(() => x, () => -x)();",
            "def Int test(f, g)() = f() + g();"
        ), "Test_%s_\\d+_\\d+");
    }

    @Test
    public void sameAnonTwiceOnlyOneClosure() {
        Model m = testExpand(parse(
            "Int x = 1; test(() => x, () => x)();",
            "def Int test(f, g)() = f() + g();"
        ), "Test_%s_\\d+_\\d+");
        FunctionDecl function = getFunction(m, Pattern.compile(expandedName("Test_%s_\\d+_\\d+")));
        assertNotNull(function);
        assertEquals(1, function.getNumParam());
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

}
