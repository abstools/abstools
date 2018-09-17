/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.ext;

import org.abs_models.common.CompilerUtils;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.FnApp;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.NewExp;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.ast.PureExp;
import org.abs_models.frontend.ast.Stmt;
import org.abs_models.frontend.ast.VarOrFieldUse;
import org.abs_models.frontend.ast.VarUse;
import org.abs_models.frontend.typechecker.DataTypeType;
import org.abs_models.frontend.typechecker.KindedName;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.UnknownType;

public class SchedulerChecker extends DefaultTypeSystemExtension {

    protected SchedulerChecker(Model m) {
        super(m);
    }

    private void checkScheduleExp(PureExp sched, ClassDecl class_decl, ASTNode<?> loc) {
        if (sched == null) return;
        if (!(sched instanceof FnApp)) {
            errors.add(new TypeError(loc, ErrorMessage.WRONG_SCHEDULER_ANNOTATION_TYPE,
                                     sched.getType()));
            return;
        }
        FnApp s = (FnApp) sched;
        Type scheduler_type = s.getType();
        if (s.getDecl().isUnknown()) {
            errors.add(new TypeError(loc, ErrorMessage.FUNCTION_NOT_RESOLVABLE,
                                     s.getName()));
            return;
        }
        FunctionDecl sd = (FunctionDecl)s.getDecl();
        // check scheduling function return type
        boolean schedulerTypeCorrect = scheduler_type.isDataType()
            && ((DataTypeType)scheduler_type).getQualifiedName().equals("ABS.Scheduler.Process");
        // check scheduling function first arg, pt.1: are we a list?
        boolean schedulerFunFirstArgCorrect = sd.getNumParam() > 0 &&
            sd.getParam(0).getType().getQualifiedName().equals("ABS.StdLib.List");
        if (schedulerFunFirstArgCorrect) {
            // check scheduling function first arg, pt.2: are we a list of
            // processes?
            DataTypeType firstArgType = (DataTypeType)sd.getParam(0).getType();
            if (firstArgType.numTypeArgs() != 1) {
                // should not happen since ABS.StdLib.List takes 1 argument
                schedulerFunFirstArgCorrect = false;
            } else {
                schedulerFunFirstArgCorrect = firstArgType.getTypeArg(0).getQualifiedName().equals("ABS.Scheduler.Process");
            }
        }
        if (!schedulerTypeCorrect || !schedulerFunFirstArgCorrect) {
            // emit two messages: one at the annotation location, one for the
            // offending scheduler function
            errors.add(new TypeError(loc, ErrorMessage.WRONG_SCHEDULER_ANNOTATION_TYPE,
                                     "dummy"));
            errors.add(new TypeError(sd, ErrorMessage.WRONG_SCHEDULER_FUN_TYPE,
                                     s.getName()));
        }
        if (s.getNumParam() == 0 || !(s.getParam(0) instanceof VarUse)
            || !((VarUse)s.getParam(0)).getName().equals("queue")) {
            // first arg to the scheduler expression must be the magic `queue'
            errors.add(new TypeError(loc,
                                     ErrorMessage.WRONG_SCHEDULER_FIRST_ARGUMENT,
                                     "dummy"));
        }
        if (s.getNumParam() != sd.getNumParam()) {
            errors.add(new TypeError(loc, ErrorMessage.WRONG_NUMBER_OF_ARGS,
                                     s.getNumParam(), sd.getNumParam()));
        } else {
            // start from 1; magic first parameter `queue' already checked
            for (int i = 1; i < s.getNumParam(); i++) {
                PureExp arg = s.getParam(i);
                String argname = "";
                if (!(arg instanceof VarOrFieldUse)) {
                    // argument was not a plain identifier
                    errors.add(new TypeError(arg, ErrorMessage.WRONG_SCHEDULER_FIELD_ARGUMENT,
                                             Integer.toString(i + 1),
                                             class_decl.getName()));
                } else {
                    // Check the rest of the parameters against class
                    // field/param names and argument types of the scheduling
                    // function.  Parts of this could be elided if we verify
                    // that `VarUse's in scheduler annotation are rewritten to
                    // `FieldUse's -- then we'd just have to check for the
                    // presence of `VarUse' in the parameter list to detect
                    // invalid args.  But can't hurt to open-code it (and we
                    // still have to check the type of all arguments vs the
                    // scheduling function parameters).
                    VarOrFieldUse vararg = (VarOrFieldUse)arg;
                    String name = vararg.getName();
                    Type argtype = UnknownType.INSTANCE;
                    for (ParamDecl p : class_decl.getParamList()) {
                        if (p.getName().equals(name)) argtype = p.getType();
                    }
                    for (FieldDecl f : class_decl.getFieldList()) {
                        if (f.getName().equals(name)) argtype = f.getType();
                    }
                    if (argtype.isUnknownType()) {
                        // identifier, but unknown in the class
                        errors.add(new TypeError(arg, ErrorMessage.WRONG_SCHEDULER_FIELD_ARGUMENT,
                                                 "\"" + name + "\"",
                                                 class_decl.getName()));
                    } else {
                        // argtype: field; paramtype: function arg
                        Type paramtype = sd.getParam(i).getType();
                        if (!argtype.isAssignableTo(paramtype)) {
                            errors.add(new TypeError(arg, ErrorMessage.TYPE_MISMATCH, argtype, paramtype));
                        }
                    }
                }
            }
        }
        if (class_decl.getType().isDeploymentComponentType()) {
            errors.add(new TypeError(loc, ErrorMessage.SCHEDULER_ON_DC,
                                     "dummy"));
        }
    }

    @Override
    public void checkNewExp(NewExp e) {
        Stmt stmt = CompilerUtils.findStmtForExpression(e);
        PureExp sched = CompilerUtils.getAnnotationValueFromName(stmt.getAnnotations(), "ABS.Scheduler.Scheduler");
        if (sched != null) {
            ClassDecl decl = (ClassDecl)(e.lookup(new KindedName(KindedName.Kind.CLASS,e.getClassName())));
            checkScheduleExp(sched, decl, stmt);
        }
    }

    @Override
    public void checkClassDecl(ClassDecl decl) {
        PureExp sched = CompilerUtils.getAnnotationValueFromName(decl.getAnnotations(), "ABS.Scheduler.Scheduler");
        checkScheduleExp(sched, decl, decl);
    }

}
