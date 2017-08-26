package abs.frontend.pardef;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import abs.backend.prettyprint.DefaultABSFormatter;
import abs.backend.prolog.ReachabilityInformation;
import abs.common.NotImplementedYetException;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.PartialFunctionDecl;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import org.junit.Ignore;
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

    private PartialFunctionDecl getPartialFunction(Model model, String name) {
        for (PartialFunctionDecl func : TreeUtil.findChildren(model, PartialFunctionDecl.class)) {
            if (func.getName().equals(name)) {
                return func;
            }
        }
        return null;
    }

    @Test
    public void unusedFunctionNotRemoved() {
        Model model = assertParseOk("def Int f()() = 0;");
        model.expandPartialFunctions();
        assertFalse(TreeUtil.isAbsent(model, PartialFunctionDecl.class));
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

    @Ignore("This isn't checked anymore since the ModuleDecl function lookup is used.")
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

    @Test
    public void noErlangCodeGenerated() throws IOException, NotImplementedYetException {
        Model model = expand(parse(
            "apply<Int, Int>(0)(inc)",
            applyFunction(),
            incFunction()
        ));
        PartialFunctionDecl func = getPartialFunction(model, "apply");
        assertNotNull(func);
        func.generateErlangCode(null);
    }

    @Test
    public void noJavaCodeGenerated() throws NotImplementedYetException, UnsupportedEncodingException {
        Model model = expand(parse(
            "apply<Int, Int>(0)(inc)",
            applyFunction(),
            incFunction()
        ));
        PartialFunctionDecl func = getPartialFunction(model, "apply");
        assertNotNull(func);

        ByteArrayOutputStream os = new ByteArrayOutputStream();
        func.generateJava(new PrintStream(os));
        assertEquals("", os.toString("utf-8"));
    }

    @Test
    public void noDynamicJavaCodeGenerated() throws NotImplementedYetException, UnsupportedEncodingException {
        Model model = expand(parse(
            "apply<Int, Int>(0)(inc)",
            applyFunction(),
            incFunction()
        ));
        PartialFunctionDecl func = getPartialFunction(model, "apply");
        assertNotNull(func);

        ByteArrayOutputStream os = new ByteArrayOutputStream();
        func.generateJavaDynamic(new PrintStream(os));
        assertEquals("", os.toString("utf-8"));
    }

    @Test
    public void noMaudeCodeGenerated() throws NotImplementedYetException, UnsupportedEncodingException {
        Model model = expand(parse(
            "apply<Int, Int>(0)(inc)",
            applyFunction(),
            incFunction()
        ));
        PartialFunctionDecl func = getPartialFunction(model, "apply");
        assertNotNull(func);

        ByteArrayOutputStream os = new ByteArrayOutputStream();
        func.generateMaude(new PrintStream(os));
        assertEquals("", os.toString("utf-8"));
    }

    @Test
    public void noPrologCodeGenerated() throws NotImplementedYetException, UnsupportedEncodingException {
        Model model = expand(parse(
            "apply<Int, Int>(0)(inc)",
            applyFunction(),
            incFunction()
        ));
        PartialFunctionDecl func = getPartialFunction(model, "apply");
        assertNotNull(func);

        ByteArrayOutputStream os = new ByteArrayOutputStream();
        func.generateProlog(new PrintStream(os), new ReachabilityInformation(new ArrayList<ASTNode<?>>()));
        assertEquals("", os.toString("utf-8"));
    }

    @Test
    public void prettyPrintImplemented() throws NotImplementedYetException, IOException {
        Model model = expand(parse(
            "apply<Int, Int>(0)(inc)",
            applyFunction(),
            incFunction()
        ));
        PartialFunctionDecl func = getPartialFunction(model, "apply");
        assertNotNull(func);

        try (StringWriter writer = new StringWriter();
            PrintWriter pw = new PrintWriter(writer)) {
            func.prettyPrint(pw, new DefaultABSFormatter(pw));
            assertFalse(writer.toString().isEmpty());
        }
    }
}
