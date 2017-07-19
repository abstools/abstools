package abs.frontend.pardef;

import static org.junit.Assert.assertNotNull;

import abs.frontend.ast.FnApp;
import abs.frontend.ast.Model;
import org.junit.Test;

public class ParFnAppTest extends PardefTest {

    private FnApp expandFunction(Model model, String expandedName) {
        return getCall(expand(model), expandedName);
    }

    private FnApp assertHasCall(Model model, String expectedName) {
        FnApp result = getCall(model, expectedName);
        String errorMessage = "No expanded function call with name " + expectedName + " created"
            + " (functions: " + getFunctions(model) + ")";
        assertNotNull(errorMessage, result);
        return result;
    }

    private void testExpand(Model model, String... expectedNames) {
        for (String expectedName : expectedNames) {
            assertHasCall(expand(model), expandedName(expectedName));
        }
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
}
