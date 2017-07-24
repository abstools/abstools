package abs.frontend.pardef;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import org.junit.Test;

public class PartialFunctionTest extends PardefTest {

    private FunctionDecl assertHasFunction(Model model, String expectedName) {
        FunctionDecl result = getFunction(model, expectedName);
        String errorMessage = "No expanded function with name " + expectedName + " created"
            + " (functions: " + getFunctions(model) + ")";
        assertNotNull(errorMessage, result);
        return result;
    }

    private void testExpand(Model model, String... expectedNames) {
        for (String expectedName : expectedNames) {
            assertHasFunction(expand(model), expandedName(expectedName));
        }
    }

    @Test
    public void unusedFunctionRemoved() {
        Model model = assertParseOk("def Int f()() = 0;");
        model.expandPartialFunctions();
        assertNull(getFunction(model));
    }

    @Test
    public void functionCorrectlyExpanded() {
        testExpand(parse("f()()", "def Int f()() = 1"), "F_%s");
        testExpand(parse("f(1)()", "def Int f(Int i)() = i"), "F_%s");
        testExpand(parse(
            "Int result = f(10)(inc)",
            incFunction(),
            "def Int f(Int i)(f_1) = f_1(i)"
        ), "F_%s_inc");
        testExpand(parse(
            "multi_param()(inc, dec)",
            incFunction(),
            decFunction(),
            "def Int multi_param()(f1, f2) = f1(f2(0))"
        ), "Multi_param_%s_inc_dec");
    }

    @Test
    public void definitionContainsParFnApp() {
        testExpand(parse(
            "Int i = outer(0)()",
            "def Int outer(Int i)() = inner(i)()",
            "def Int inner(Int i)() = i * 2"
        ), "Outer_%s", "Inner_%s");
    }

    @Test(expected = PardefModellingException.class)
    public void duplicateFunction() {
        expand(parse(
            "inc(1)",
            incFunction(),
            applyFunction(),
            applyFunction()
        ));
    }

    @Test
    public void passFuncParam() {
        testExpand(parse(
            "test(1)(inc)",
            incFunction(),
            applyFunction(),
            "def Int test(Int i)(f) = apply<Int, Int>(i)(f)"
        ), "Test_%s_inc", "Apply_%s_inc_Int_Int");
    }

    @Test
    public void callInnerParametricWithTypeParams() {
        testExpand(parse(
            "test<Int, Int>(1)()",
            applyFunction(),
            incFunction(),
            "def B test<A, B>(A a)() = apply<A, B>(a)(inc)"
        ), "Test_%s_Int_Int", "Apply_%s_inc_Int_Int");
    }
}
