/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import abs.common.CompilerUtils;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Exp;
import abs.frontend.ast.ExpressionStmt;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.VarDeclStmt;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.Type;

public class SchedulerChecker extends DefaultTypeSystemExtension {

    protected SchedulerChecker(Model m) {
        super(m);
    }

    private void checkScheduleExp(PureExp sched, ASTNode<?> loc) {
        if (sched == null) return;
        if (!(sched instanceof FnApp)) {
            errors.add(new TypeError(loc, ErrorMessage.WRONG_SCHEDULER_TYPE,
                                     sched.getType()));
        } else {
            FnApp s = (FnApp) sched;
            Type t = s.getType();
            if (!(t.isDataType() && ((DataTypeType)t).getSimpleName().equals("Process"))) {
                errors.add(new TypeError(loc, ErrorMessage.WRONG_SCHEDULER_TYPE,
                                         sched.getType()));
            }
            // TODO: type-check arguments of function call
        }
    }

    @Override
    public void checkClassDecl(ClassDecl decl) {
        PureExp sched = CompilerUtils.getAnnotationValueFromName(decl.getAnnotations(), "ABS.Scheduler.Scheduler");
        checkScheduleExp(sched, decl);
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt e) {
        Exp exp = e.getExp();
        if (exp instanceof NewExp) {
            PureExp sched = CompilerUtils.getAnnotationValueFromName(e.getAnnotations(), "ABS.Scheduler.Scheduler");
            checkScheduleExp(sched, e);
        }
    }
    
    @Override
    public void checkAssignStmt(AssignStmt s) {
        Exp exp = s.getValue();
        if (exp instanceof NewExp) {
            PureExp sched = CompilerUtils.getAnnotationValueFromName(s.getAnnotations(), "ABS.Scheduler.Scheduler");
            checkScheduleExp(sched, s);
        }
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt v) {
        if (!v.getVarDecl().hasInitExp()) return;
        Exp exp = v.getVarDecl().getInitExp();
        if (exp instanceof NewExp) {
            PureExp sched = CompilerUtils.getAnnotationValueFromName(v.getAnnotations(), "ABS.Scheduler.Scheduler");
            checkScheduleExp(sched, v);
        }
    }

}
