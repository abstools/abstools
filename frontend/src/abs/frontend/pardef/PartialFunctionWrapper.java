package abs.frontend.pardef;

import abs.frontend.ast.Access;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.IntLiteral;
import abs.frontend.ast.ParFnApp;
import abs.frontend.ast.PartialFunctionDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.TypedAnnotation;

public final class PartialFunctionWrapper {

    private final PartialFunctionDecl decl;
    private int applyCounter;

    public PartialFunctionWrapper(PartialFunctionDecl decl) {
        this.decl = decl;
        this.applyCounter = 0;
    }

    public FunctionDecl apply(ParFnApp fnApp, Annotation annotation) {
        return decl.apply(fnApp, annotation, applyCounter++);
    }

    public PartialFunctionDecl getFunction() {
        return decl;
    }

    public void removeFromParent() {
        TreeUtil.removeFromParent(getFunction());
    }
}
