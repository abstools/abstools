package abs.frontend.pardef;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import abs.frontend.ast.Decl;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.PartialFunctionDecl;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.KindedName.Kind;
import java.util.regex.Pattern;

/**
 * A base class for test classes testing the correct expansion of partial function definitions.
 */
public abstract class AbstractPartialFunctionTest extends PardefTest {

    protected FunctionDecl assertHasFunction(Model model, String regex) {
        FunctionDecl result = getFunction(model, Pattern.compile(regex));
        String errorMessage = "No expanded function with name " + regex + " created"
            + " (functions: " + getFunctions(model) + ")";
        assertNotNull(errorMessage, result);
        Decl decl = model.lookup(new KindedName(Kind.FUN, result.getName()));
        assertFalse("Could not lookup function " + result.getName(), decl.isUnknown());
        return result;
    }

    protected Model testExpand(Model model, String... expectedNames) {
        model = expand(model);
        for (String expectedName : expectedNames) {
            assertHasFunction(model, expandedName(expectedName));
        }
        return model;
    }

    protected PartialFunctionDecl getPartialFunction(Model model, String regex) {
        Pattern pattern = Pattern.compile(regex);
        for (PartialFunctionDecl func : model.findChildren(PartialFunctionDecl.class)) {
            if (pattern.matcher(func.getName()).matches()) {
                return func;
            }
        }
        return null;
    }
}
