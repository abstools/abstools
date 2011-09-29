/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import abs.backend.maude.MaudeCompilerHelper;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.PureExp;

public class SchedulerChecker extends DefaultTypeSystemExtension {

    protected SchedulerChecker(Model m) {
        super(m);
    }

    @Override
    public void checkClassDecl(ClassDecl decl) {
        PureExp sched = MaudeCompilerHelper.getAnnotationValue(decl.getAnnotations(), "Scheduler");
        if (sched != null) {
            // errors.add(new TypeError(n, ErrorMessage.WRONG_DEADLINE_TYPE,
            //                          deadline.getType().getQualifiedName()));
            
        }
    }

}
