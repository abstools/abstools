/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import abs.common.CompilerUtils;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.FnApp;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.Type;



public class SchedulerChecker extends DefaultTypeSystemExtension {

    protected SchedulerChecker(Model m) {
        super(m);
    }

    @Override
    public void checkClassDecl(ClassDecl decl) {
        PureExp sched = CompilerUtils.getAnnotationValue(decl.getAnnotations(), "Scheduler");
        if (sched != null) {
            if (!(sched instanceof FnApp)) {
                errors.add(new TypeError(decl, ErrorMessage.WRONG_SCHEDULER_TYPE,
                                         sched.getType()));
            } else {
                FnApp s = (FnApp) sched;
                Type t = s.getType();
                if (!(t.isDataType() && ((DataTypeType)t).getSimpleName().equals("Process"))) {
                    errors.add(new TypeError(decl, ErrorMessage.WRONG_SCHEDULER_TYPE,
                            sched.getType()));
                }
                // TODO: type-check arguments of function call
            }
        }
    }

}
