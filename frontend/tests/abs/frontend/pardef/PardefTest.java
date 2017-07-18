package abs.frontend.pardef;

import abs.frontend.FrontendTest;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import java.util.Iterator;
import java.util.List;

public abstract class PardefTest extends FrontendTest {

    private static final String MODULE_NAME = "UnitTest";

    protected String applyFunction() {
        return "def B apply<A, B>(A a)(f) = f(a)";
    }

    protected String incFunction() {
        return "def Int inc(Int i) = i + 1";
    }

    protected String decFunction() {
        return "def Int dec(Int i) = i - 1";
    }

    protected String halveFunction() {
        return "def Rat halve(Int i) = i / 2";
    }

    protected final String getFunctions(Model model) {
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

    protected final FunctionDecl getFunction(Model model) {
        List<FunctionDecl> functions = TreeUtil.findChildren(model, FunctionDecl.class);
        if (functions.isEmpty()) {
            return null;
        } else {
            return functions.get(0);
        }
    }

    protected final FunctionDecl getFunction(Model model, String name) {
        for (FunctionDecl decl : TreeUtil.findChildren(model, FunctionDecl.class)) {
            if (decl.getName().equals(name)) {
                return decl;
            }
        }
        return null;
    }

    protected final FnApp getCall(Model model, String name) {
        for (FnApp fnApp : TreeUtil.findChildren(model, FnApp.class)) {
            if (fnApp.getName().equals(name)) {
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
        return model;
    }

    protected final Model parse(String functionCall, String... functions) {
        StringBuilder builder = new StringBuilder(functions.length * 30);
        for (String function : functions) {
            builder.append(function).append(';');
        }

        builder.append("{ ")
            .append(functionCall).append(';')
            .append(" }");

        return assertParseOk(builder.toString());
    }
}
