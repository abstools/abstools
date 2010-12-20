package abs.frontend.typechecker.ext;

import java.util.List;

import abs.frontend.analyser.AnnotationHelper;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.AwaitStmt;
import abs.frontend.ast.Call;
import abs.frontend.ast.GetExp;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.SuspendStmt;

public class AtomicityChecker extends DefaultTypeSystemExtension {

    protected AtomicityChecker(Model m) {
        super(m);
    }

    @Override
    public void checkAwaitStmt(AwaitStmt s) {
        ensureNonAtomic(s,s.getContextMethod().getMethodSig(),"an await statement");
    }

    @Override
    public void checkSuspendStmt(SuspendStmt s) {
        ensureNonAtomic(s,s.getContextMethod().getMethodSig(),"a suspend statement");
    }

    @Override
    public void checkGetExp(GetExp e) {
        ensureNonAtomic(e,e.getContextMethod().getMethodSig(),"a suspend statement");
    }
    
    @Override
    public void checkMethodCall(Call call) {
        if (!call.isAsync()) {
            MethodSig sig = call.getMethodSig();
            if (!isAtomic(sig.getAnnotations())) {
                ensureNonAtomic(call, call.getContextMethod().getMethodSig(), "a synchronous call of a non-atomic method");
            }
        }
    }
    
    private void ensureNonAtomic(ASTNode<?> n, MethodSig sig, String descr) {
        if (isAtomic(sig.getAnnotations())) {
            errors.add(new TypeError(n, ErrorMessage.ATOMIC_METHOD_CONTAINS_ILLEGAL_CODE,descr,sig.getName()));
        }
    }
    
    private boolean isAtomic(abs.frontend.ast.List<Annotation> list) {
        List<Annotation> anns = AnnotationHelper.getAnnotationsOfType(list, "ABS.StdLib.AtomicityAnnotation");
        return !anns.isEmpty();
        
    }
    
}
