package abs.frontend.pardef;

import static org.junit.Assert.assertFalse;

import abs.frontend.FrontendTest;
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

public abstract class PardefTest extends FrontendTest {

    private static final String MODULE_NAME = "UnitTest";

    protected String applyFunction() {
        return "def B apply<A, B>(f)(A a) = f(a);";
    }

    protected String incFunction() {
        return "def Int inc(Int i) = i + 1;";
    }

    protected String decFunction() {
        return "def Int dec(Int i) = i - 1;";
    }

    protected String halveFunction() {
        return "def Rat halve(Int i) = i / 2;";
    }

    protected final String getFunctions(Model model) {
        StringBuilder builder = new StringBuilder().append('[');
        Iterator<FunctionDecl> iterator = model.findChildren(FunctionDecl.class).iterator();
        while (iterator.hasNext()) {
            FunctionDecl decl = iterator.next();
            builder.append(decl.getName());
            if (iterator.hasNext()) {
                builder.append(", ");
            }
        }
        return builder.append(']').toString();
    }

    protected final FunctionDecl getFunction(Model model, Pattern regex) {
        for (FunctionDecl decl : model.findChildren(FunctionDecl.class)) {
            if (regex.matcher(decl.getName()).matches()) {
                return decl;
            }
        }
        return null;
    }

    protected final FnApp getCall(Model model, Pattern regex) {
        for (FnApp fnApp : model.findChildren(FnApp.class)) {
            if (regex.matcher(fnApp.getName()).matches()) {
                return fnApp;
            }
        }
        return null;
    }

    protected final String expandedName(String expandedName) {
        return String.format(expandedName, MODULE_NAME);
    }

    protected final Model expand(Model model) {
        model.expandPartialFunctions();
        SemanticConditionList e = model.typeCheck();
        assertFalse("Type check errors! First: " + e.getFirstError(), e.containsErrors());
        return model;
    }

    protected final Model parse(String functionCall, String... functions) {
        return parse(true, functionCall, functions);
    }

    protected final Model parse(boolean withStbLib, String functionCall, String... functions) {
        StringBuilder builder = new StringBuilder(functions.length * 30);
        for (String function : functions) {
            builder.append(function);
        }

        builder.append("{ ")
            .append(functionCall)
            .append(" }");

        String code = builder.toString();
        return withStbLib ? assertParseOkStdLib(code) : assertParseOk(code);
    }
}
