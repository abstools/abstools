package org.abs_models.frontend.pardef;

import static org.junit.Assert.assertFalse;

import org.abs_models.backend.prettyprint.DefaultABSFormatter;
import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.FnApp;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.Model;
import com.google.common.base.Joiner;

import java.io.PrintWriter;
import java.util.LinkedList;
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
        List<FunctionDecl> functions = model.findChildren(FunctionDecl.class);
        // The desired function is probably at the end, so we reverse the list
        LinkedList<String> reversedNames = new LinkedList<>();
        for (FunctionDecl fun : functions) {
            reversedNames.addFirst(fun.getName());
        }
        return '[' + Joiner.on(", ").join(reversedNames) + ']';
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
        try {
            model.flattenTraitOnly();
            model.expandPartialFunctions();
            SemanticConditionList e = model.typeCheck();
            assertFalse("Type check errors! First: " + e.getFirstError(), e.containsErrors());
            return model;
        } catch (Throwable e) {
            if (e instanceof PardefModellingException) {
                // prettyprint could fail if expansion left the AST in an invalid state
                throw e;
            }
            PrintWriter pw = new PrintWriter(System.out);
            model.lookupModule("UnitTest").doPrettyPrint(pw, new DefaultABSFormatter(pw));
            pw.flush();
            throw e;
        }
    }

    protected final Model parse(String functionCall, String... functions) {
        StringBuilder builder = new StringBuilder(functions.length * 30);
        for (String function : functions) {
            builder.append(function);
        }

        builder.append("{ ")
            .append(functionCall)
            .append(" }");

        String code = builder.toString();
        return assertParse(code);
    }
}
