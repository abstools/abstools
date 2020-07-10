/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.ext;

import java.util.List;

import org.abs_models.frontend.analyser.AnnotationHelper;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.Annotation;
import org.abs_models.frontend.ast.AwaitStmt;
import org.abs_models.frontend.ast.AwaitAsyncCall;
import org.abs_models.frontend.ast.Call;
import org.abs_models.frontend.ast.GetExp;
import org.abs_models.frontend.ast.MethodImpl;
import org.abs_models.frontend.ast.MethodSig;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.SuspendStmt;

public class AtomicityChecker extends DefaultTypeSystemExtension {

    protected AtomicityChecker(Model m) {
        super(m);
    }

    @Override
    public void checkOverride(MethodSig impl, MethodSig overriden) {
        if (impl.isAtomic() != overriden.isAtomic()) {
            errors.add(new TypeError(impl, ErrorMessage.ATOMIC_METHOD_WRONG_OVERRIDE,impl.getName(),impl.getName(),overriden.getContextDecl().getName()));
        }
    }
    
    @Override
    public void checkAwaitStmt(AwaitStmt s) {
        ensureNonAtomic(s,s.getContextMethod(),"an await statement");
    }

    @Override
    public void checkSuspendStmt(SuspendStmt s) {
        ensureNonAtomic(s,s.getContextMethod(),"a suspend statement");
    }

    @Override
    public void checkGetExp(GetExp e) {
        ensureNonAtomic(e,e.getContextMethod(),"a blocking get expression");
    }
    
    @Override
    public void checkMethodCall(Call call) {
        if (!call.isAsync()) {
            MethodSig sig = call.getMethodSig();
            if (!isAtomic(sig.getAnnotations())) {
                ensureNonAtomic(call, call.getContextMethod(), "a synchronous call to non-atomic method " + sig.getName());
            }
        } else if (call instanceof AwaitAsyncCall) {
            // This is for the case when AwaitAsyncCall rewriting is turned
            // off (e.g., in the Maude backend); otherwise, this case is
            // caught by the `await' and `get' changes elsewhere in this file.
            ensureNonAtomic(call, call.getContextMethod(), "an await expression");
        }
    }
    
    private void ensureNonAtomic(ASTNode<?> n, MethodImpl impl, String descr) {
        if (impl == null)
            return;
        MethodSig sig = impl.getMethodSig();
        if (sig.isAtomic()) {
            errors.add(new TypeError(n, ErrorMessage.ATOMIC_METHOD_CONTAINS_ILLEGAL_CODE,descr,sig.getName()));
        }
    }
    
    public static boolean isAtomic(org.abs_models.frontend.ast.List<Annotation> list) {
        List<Annotation> anns = AnnotationHelper.getAnnotationsOfType(list, "ABS.StdLib.AtomicityAnnotation");
        return !anns.isEmpty();
        
    }
    
}
