package abs.frontend.pardef;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.ParFnApp;
import abs.frontend.ast.PartialFunctionDecl;

public final class PartialFunctionWrapper {

    private final PartialFunctionDecl decl;
    private int applyCounter;

    public PartialFunctionWrapper(PartialFunctionDecl decl) {
        this.decl = decl;
        this.applyCounter = 0;
    }

    public FunctionDecl apply(ParFnApp fnApp) {
        return decl.apply(fnApp, applyCounter++);
    }

    public PartialFunctionDecl getFunction() {
        return decl;
    }

    public void removeFromParent() {
        TreeUtil.removeFromParent(getFunction());
    }
}
