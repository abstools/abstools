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

    private boolean isSideEffectExpContainer(EObject statement) {
        // We allow the following forms:
        // - "f.get;" (ExpressionStatement)
        // - "x = f.get;" (AssignStatement)
        // - "T x = f.get" (VarDeclStatement)
        // - "return f.get" (ReturnStatement)
        return statement instanceof ExpressionStatement
            || statement instanceof AssignStatement
            || statement instanceof VariableDeclarationStatement
            || statement instanceof ReturnStatement;
    }
    private boolean isSingleMethodCallGuard(Guard g) {
        return g instanceof ExpressionGuard
            && ((ExpressionGuard)g).getExpression() instanceof MethodCallExpression;
    }

    @Check
    public void checkGetExpPlacement(GetExpression e) {
        if(!isSideEffectExpContainer(e.eContainer())) {
            error("A get expression cannot be a sub-expression",
                  e.eContainer(), e.eContainingFeature());
        }
    }
    @Check
    public void checkOriginalCallExpPlacement(OriginalCallExpression e) {
        if(!isSideEffectExpContainer(e.eContainer())) {
            error("An original call expression cannot be a sub-expression",
                  e.eContainer(), e.eContainingFeature());
        }
    }
    @Check
    public void checkNewExpPlacement(NewExpression e) {
        if(!isSideEffectExpContainer(e.eContainer())) {
            error("A new expression cannot be a sub-expression",
                  e.eContainer(), e.eContainingFeature());
        }
    }
    @Check
    public void checkAwaitExpPlacement(AwaitExpression e) {
        if (isSingleMethodCallGuard(e.getGuard())) {
            // "await o!m()", i.e., an await expression
            if(!isSideEffectExpContainer(e.eContainer())) {
                error("An await expression cannot be a sub-expression",
                      e.eContainer(), e.eContainingFeature());
            }
        } else {
            // any other await, i.e., an await statement
            if (!(e.eContainer() instanceof ExpressionStatement)) {
                error("Invalid placement of await statement",
                      e.eContainer(), e.eContainingFeature());
            }
        }
    }
    @Check
    public void checkValidAwaitCall(MethodCallExpression e) {
        if (e.eContainer() instanceof ExpressionGuard
            && ((MethodCallExpression)e).getOperator().equals(".")) {
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
        if (l instanceof ExpressionGuard && ((ExpressionGuard)l).getExpression() instanceof MethodCallExpression) {
            error("A side-effect expression cannot be a sub-expression",
                  AbsPackage.eINSTANCE.getAndGuard_Left());
        }
        if (r instanceof ExpressionGuard && ((ExpressionGuard)r).getExpression() instanceof MethodCallExpression) {
            error("A side-effect expression cannot be a sub-expression",
                  AbsPackage.eINSTANCE.getAndGuard_Left());
        }
    }

    @Check
    public void checkAllDeclarationsSimple(ModuleDeclaration m) {
        for (Declaration d : m.getDeclarations()) {
            checkSimpleName(d);
        }
    }
    private void checkSimpleName(Declaration d) {
        if (d.getDatatypeDeclaration() != null) {
            if (d.getDatatypeDeclaration().getName().indexOf('.') != -1) {
                error("Cannot use a qualified name to name a datatype",
                      d.getDatatypeDeclaration(),
                      AbsPackage.eINSTANCE.getDataTypeDeclaration_Name());
            }
        } else if (d.getTypesynonymDeclaration() != null) {
            if (d.getTypesynonymDeclaration().getName().indexOf('.') != -1) {
                error("Cannot use a qualified name to name a type synonym",
                      d.getTypesynonymDeclaration(),
                      AbsPackage.eINSTANCE.getTypesynonymDeclaration_Name());
            }
        } else if (d.getExceptionDeclaration() != null) {
            if (d.getExceptionDeclaration().getName().indexOf('.') != -1) {
                error("Cannot use a qualified name to name an exception",
                      d.getExceptionDeclaration(),
                      AbsPackage.eINSTANCE.getExceptionDeclaration_Name());
            }
        } else if (d.getFunctionDeclaration() != null) {
            if (d.getFunctionDeclaration().getName().indexOf('.') != -1) {
                error("Cannot use a qualified name to name a function",
                      d.getFunctionDeclaration(),
                      AbsPackage.eINSTANCE.getFunctionDeclaration_Name());
            }
        } else if (d.getTraitDeclaration() != null) {
            if (d.getTraitDeclaration().getName().indexOf('.') != -1) {
                error("Cannot use a qualified name to name a trait",
                      d.getTraitDeclaration(),
                      AbsPackage.eINSTANCE.getTraitDeclaration_Name());
            }
        } else if (d.getInterfaceDeclaration() != null) {
            if (d.getInterfaceDeclaration().getName().indexOf('.') != -1) {
                error("Cannot use a qualified name to name an interface",
                      d.getInterfaceDeclaration(),
                      AbsPackage.eINSTANCE.getInterfaceDeclaration_Name());
            }
        } else if (d.getClassDeclaration() != null) {
            if (d.getClassDeclaration().getName().indexOf('.') != -1) {
                error("Cannot use a qualified name to name a class",
                      d.getClassDeclaration(),
                      AbsPackage.eINSTANCE.getClassDeclaration_Name());
            }
        }
    }
}
