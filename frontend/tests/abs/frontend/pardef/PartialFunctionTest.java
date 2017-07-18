package abs.frontend.pardef;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import abs.frontend.FrontendTest;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import java.util.Iterator;
import java.util.List;
import org.junit.Test;

public class PartialFunctionTest extends FrontendTest {

    private static final String MODULE_NAME = "UnitTest";

    private String getFunctions(Model model) {
        StringBuilder builder = new StringBuilder().append('[');
        Iterator<FunctionDecl> iterator = TreeUtil.findChildren(model, FunctionDecl.class).iterator();
        while (iterator.hasNext()) {
            FunctionDecl decl = iterator.next();
            builder.append(decl.getName());
            if (iterator.hasNext()) {
                builder.append(", ");
            }
        }
        return builder.append(']').toString();
    }

    private FunctionDecl getFunction(Model model) {
        List<FunctionDecl> functions = TreeUtil.findChildren(model, FunctionDecl.class);
        if (functions.isEmpty()) {
            return null;
        } else {
            return functions.get(0);
        }
    }

    private FunctionDecl getFunction(Model model, String name) {
        for (FunctionDecl decl : TreeUtil.findChildren(model, FunctionDecl.class)) {
            if (decl.getName().equals(name)) {
                return decl;
            }
        }
        return null;
    }

    private FunctionDecl testExpand(Model model, String expectedName) {
        model.expandPartialFunctions();
        expectedName = String.format(expectedName, MODULE_NAME);
        FunctionDecl result = getFunction(model, expectedName);
        String errorMessage = "No expanded function with name " + expectedName + " created"
            + " (functions: " + getFunctions(model) + ")";
        assertNotNull(errorMessage, result);
        return result;
    }

    private Model parse(String functionCall, String... functions) {
        StringBuilder builder = new StringBuilder(functions.length * 30);
        for (String function : functions) {
            builder.append(function).append(';');
        }

        builder.append("{ ")
            .append(functionCall).append(';')
            .append(" }");

        return assertParseOk(builder.toString());
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
            "def Int inc(Int i) = i + 1",
            "def Int f(Int i)(f_1) = f_1(i)"
        ), "F_%s_inc");
        testExpand(parse(
            "multi_param()(inc, dec)",
            "def Int inc(Int i) = i + 1",
            "def Int dec(Int i) = i - 1",
            "def Int multi_param()(f1, f2) = f1(f2(0))"
        ), "Multi_param_%s_inc_dec");
    }

    @Test(expected = PardefModellingException.class)
    public void parametricCallWithoutParamsNotPossible() {
        testExpand(parse(
            "apply(2)(halve)",
            "def Rat halve(Int i) = i / 2",
            "def B apply<A, B>(A a)(f) = f(a)"
        ), "Apply_%s_halve_Int_Rat");
    }


    @Test
    public void parametricCall() {
        testExpand(parse(
            "Rat r = apply<Int, Rat>(2)(halve)",
            "def Rat halve(Int i) = i / 2",
            "def B apply<A, B>(A a)(f) = f(a)"
        ), "Apply_%s_halve_Int_Rat");
        testExpand(parse(
            "List<Rat> l = map<Int, Rat>(Nil)(halve)",
            "def Rat halve(Int i) = i / 2",
            "def List<B> map<A, B>(List<A> list)(f) = case list {\n"
                + "Cons(x, xs) => Cons(f_1(x), map(xs));\n"
                + "Nil => Nil;\n"
                + "}"
        ), "Map_%s_halve_Int_Rat");
    }
}
