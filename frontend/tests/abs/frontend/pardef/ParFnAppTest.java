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
            "apply<Int, Int>(0)(inc, inc)",
            incFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooFewFuncArgs() {
        expand(parse(
            "apply<Int, Int>(0)()",
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooManyArgs() {
        expand(parse(
            "apply<Int, Int>(0, 1)(inc)",
            incFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooFewArgs() {
        expand(parse(
            "apply<Int, Int>()(inc)",
            incFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void callingUnknown() {
        expand(parse(
            "apply<Int, Int>(0)(inc)",
            incFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void simpleCycle() {
        // test -> test2 -> test
        expand(parse(
            "test()()",
            "def Int test()() = test2()()",
            "def Int test2()() = test()()"
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void bigCycle() {
        // test -> test2 -> test3 -> test
        expand(parse(
            "test()()",
            "def Int test()() = test2()()",
            "def Int test2()() = test3()()",
            "def Int test3()() = test()()"
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void indirectCycle() {
        // test -> test2 -> test3 -> test2
        expand(parse(
            "test()()",
            "def Int test()() = test2()()",
            "def Int test2()() = test3()()",
            "def Int test3()() = test2()()"
        ));
    }

    @Test
    public void recursion() {
        testExpand(parse(
            "List<Rat> l = map<Int, Rat>(Nil)(halve)",
            halveFunction(),
            "def List<B> map<A, B>(List<A> list)(f) = case list {\n"
                + "Cons(x, xs) => Cons(f_1(x), map(xs));\n"
                + "Nil => Nil;\n"
                + "}"
        ), "Map_%s_halve_Int_Rat");
    }

    @Test
    public void recursionWithClosure() {
        Model m = expand(parse(
            true,
            "Int x = 0; Int y = 1; rec()((Int i) => x, (Int j) => y)",
            "def Int rec()(f, g) = rec()"
        ));
        FnApp call = assertHasCall(m, expandedName("Rec_%s_\\d+_\\d+"));
        assertEquals(2, call.getNumParam());
    }

    @Test
    public void parametricCall() {
        testExpand(parse(
            "Rat r = apply<Int, Rat>(2)(halve)",
            halveFunction(),
            applyFunction()
        ), "Apply_%s_halve_Int_Rat");
        testExpand(parse(
            "List<Rat> l = map<Int, Rat>(Nil)(halve)",
            halveFunction(),
            "def List<B> map<A, B>(List<A> list)(f) = case list {\n"
                + "Cons(x, xs) => Cons(f_1(x), map(xs));\n"
                + "Nil => Nil;\n"
                + "}"
        ), "Map_%s_halve_Int_Rat");
    }

    @Test(expected = PardefModellingException.class)
    public void parametricCallWithoutParamsNotPossible() {
        expand(parse(
            "apply(2)(halve)",
            halveFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooFewTypeParams() {
        expand(parse(
            "apply<Int>(2)(halve)",
            halveFunction(),
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void tooManyTypeParams() {
        expand(parse(
            "apply<Int, Rat, Int>(2)(halve)",
            halveFunction(),
            applyFunction()
        ));
    }

    @Test
    public void nestedCalls() {
        testExpand(parse(
            "Int i = apply<Int, Int>(apply<Int, Int>(0)(dec))(inc)",
            incFunction(),
            decFunction(),
            applyFunction()
        ), "Apply_%s_dec_Int_Int", "Apply_%s_inc_Int_Int");
    }

    @Test(expected = PardefModellingException.class)
    public void invalidCallToAlreadyExpanded() {
        expand(parse(
            "apply<Int, Int>(0)(inc); apply<Int, Int>(0, 42)(inc)",
            incFunction(),
            applyFunction()
        ));
    }

    @Test
    public void callWithAnonymousFunction() {
        testExpand(parse(
            "apply<Int, Int>(0)((Int i) => i)",
            applyFunction()
        ));

        testExpand(parse(
            "apply<Int, Int>(0)((Int i) => i + 1)",
            applyFunction()
        ));

        testExpand(parse(
            "apply<Int, Int>(0)((Int i) => inc(i))",
            applyFunction(),
            incFunction()
        ), "Apply_%s_\\d+_Int_Int");

        testExpand(parse(
            "Int i = test()(() => 1)",
            "def Int test()(f) = f()"
        ), "Test_%s_\\d+");
    }

    @Test(expected = PardefModellingException.class)
    public void anonymousTooManyArgs() {
        expand(parse(
            "apply<Int, Int>(0)((Int i, Int j) => i)",
            applyFunction()
        ));
    }

    @Test(expected = PardefModellingException.class)
    public void anonymousTooFewArgs() {
        expand(parse(
            "apply<Int, Int>(0)(() => i)",
            applyFunction()
        ));
    }

    @Test
    public void anonymousSimpleClosure() {
        testExpand(parse(
            true,
            "Int x = 0; apply<Int, Int>(0)((Int i) => i + x)",
            applyFunction()
        ), "Apply_%s_\\d+_Int_Int");
    }

    @Test
    public void anonymousNestedClosure() {
        testExpand(parse(
            true,
            "Int x = 0; apply<Int, Int>(0)((Int i) => apply<Int, Int>(i)((Int j) => j + x))",
            applyFunction()
        ), "Apply_%s_\\d+_Int_Int");
    }

    @Test
    public void closureParamSameNameAsFunctionParam() {
        testExpand(parse(
            true,
            "Int x = 1; test(0)((Int i) => x + i)",
            "def Int test(Int x_0)(f) = f(x_0)"
        ), "Test_%s_\\d+");
    }

    @Test
    public void multipleAnonsUsingSameClosureVar() {
        testExpand(parse(
            true,
            "Int x = 1; test()(() => x, () => -x)",
            "def Int test()(f, g) = f() + g()"
        ), "Test_%s_\\d+_\\d+");
    }

    @Test
    public void sameAnonTwiceOnlyOneClosure() {
        Model m = testExpand(parse(
            true,
            "Int x = 1; test()(() => x, () => x)",
            "def Int test()(f, g) = f() + g()"
        ), "Test_%s_\\d+_\\d+");
        FunctionDecl function = getFunction(m, Pattern.compile(expandedName("Test_%s_\\d+_\\d+")));
        assertNotNull(function);
        assertEquals(1, function.getNumParam());
    }

}
