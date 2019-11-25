package org.abs_models.xtext.validation;

import com.google.inject.Inject;

import org.abs_models.xtext.abs.*;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.validation.AbstractDeclarativeValidator;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.EValidatorRegistrar;

/**
 * This class contains the custom validation rules for Core ABS.  It is
 * integrated into the xtext framework via the class AbsValidator.
 *
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation
 */
public class CoreAbsValidator extends AbstractDeclarativeValidator {
    @Override
    @Inject
    public void register(EValidatorRegistrar registrar) {
        // nothing to do
    }
    private boolean isSideEffectExpContainer(EObject stmt) {
        // We allow the following forms:
        // - "f.get;" (ExpStmt)
        // - "x = f.get;" (AssignStmt)
        // - "T x = f.get" (VarDeclStmt)
        // - "return f.get" (ReturnStmt)
        return stmt instanceof ExpStmt
            || stmt instanceof AssignStmt
            || stmt instanceof VarDeclStmt
            || stmt instanceof ReturnStmt;
    }
    private boolean isSingleMethodCallGuard(Guard g) {
        return g instanceof ExpGuard
            && ((ExpGuard)g).getExp() instanceof MethodCallExp;
    }

    @Check
    public void checkGetExpPlacement(GetExp e) {
        if(!isSideEffectExpContainer(e.eContainer())) {
            error("A get expression cannot be a sub-expression",
                  e.eContainer(), e.eContainingFeature());
        }
    }
    @Check
    public void checkOriginalCallExpPlacement(OriginalCallExp e) {
        if(!isSideEffectExpContainer(e.eContainer())) {
            error("An original call expression cannot be a sub-expression",
                  e.eContainer(), e.eContainingFeature());
        }
    }
    @Check
    public void checkNewExpPlacement(NewExp e) {
        if(!isSideEffectExpContainer(e.eContainer())) {
            error("A new expression cannot be a sub-expression",
                  e.eContainer(), e.eContainingFeature());
        }
    }
    @Check
    public void checkAwaitExpPlacement(AwaitExp e) {
        if (isSingleMethodCallGuard(e.getGuard())) {
            // "await o!m()", i.e., an await expression
            if(!isSideEffectExpContainer(e.eContainer())) {
                error("An await expression cannot be a sub-expression",
                      e.eContainer(), e.eContainingFeature());
            }
        } else {
            // any other await, i.e., an await statement
            if (!(e.eContainer() instanceof ExpStmt)) {
                error("Invalid placement of await statement",
                      e.eContainer(), e.eContainingFeature());
            }
        }
    }
    @Check
    public void checkValidAwaitCall(MethodCallExp e) {
        if (e.eContainer() instanceof ExpGuard
            && ((MethodCallExp)e).getOperator().equals(".")) {
            // disallow "await o.m()"
            error("Cannot await on synchronous method call",
                  e.eContainer(), e.eContainingFeature());
        }
    }

    @Check
    public void checkNoMethodCallInSubguard(AndGuard g) {
        // Block "await o!m() & o!m();"
        Guard l = g.getLeft();
        Guard r = g.getRight();
        if (l instanceof ExpGuard && ((ExpGuard)l).getExp() instanceof MethodCallExp) {
            error("A side-effect expression cannot be a sub-expression",
                  AbsPackage.eINSTANCE.getAndGuard_Left());
        }
        if (r instanceof ExpGuard && ((ExpGuard)r).getExp() instanceof MethodCallExp) {
            error("A side-effect expression cannot be a sub-expression",
                  AbsPackage.eINSTANCE.getAndGuard_Left());
        }
    }
}
