package org.abs_models.xtext.validation;

import com.google.inject.Inject;

import org.abs_models.xtext.abs.*;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.validation.AbstractDeclarativeValidator;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.EValidatorRegistrar;

/**
 * This class contains the custom validation rules for Full ABS.  It is
 * integrated into the xtext framework via the class AbsValidator.
 *
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation
 */
public class FullAbsValidator extends AbstractDeclarativeValidator {

    @Override
    @Inject
    public void register(EValidatorRegistrar registrar) {
        // nothing to do
    }

    // Deltas
    @Check public void checkAllowedAddedDeclaration(DeltaModuleModifier mod) {
        if (mod.getAdded_decl() != null) {
            Declaration d = mod.getAdded_decl();
            if (!(d instanceof ClassDecl
                  || d instanceof InterfaceDecl
                  || d instanceof FunctionDecl
                  || d instanceof DataTypeDecl
                  || d instanceof TypeSynonymDecl
                  )) {
                error("Adding of " + d.getClass().getName() + " declarations is unsupported",
                      AbsPackage.eINSTANCE.getDeltaModuleModifier_Added_decl());
            }
        }
    }

    @Check public void checkNoFieldInitializerForDeltaCondition(DeltaCondition d) {
        if (d.getDeltaFieldCondition() != null
            && d.getDeltaFieldCondition().isHasInit()) {
            error("Cannot use a field initializer here",
                  AbsPackage.eINSTANCE.getDeltaCondition_DeltaFieldCondition());
        }
    }

    @Check public void checkNoFieldInitializerForRemovedField(ClassModifier cm) {
        if (cm.getRemoved_field() != null
            && cm.getRemoved_field().isHasInit()) {
            error("Cannot use a field initializer here",
                  AbsPackage.eINSTANCE.getClassModifier_Removed_field());
        }
    }

    @Check public void checkOnlyTrueOrFalseDeltaParameter(AttributeAssignmentValue_Bool aavb) {
        if (!(aavb.getValue().equals("True") || aavb.getValue().equals("False"))) {
            error("Invalid Boolean value, must be ‘True’ or ‘False’.",
                  AbsPackage.eINSTANCE.getAttributeAssignmentValue_Bool_Value());
        }
    }

    @Check public void checkFeatureAttributeParameterType(MTVLAttributeDecl ad) {
        if (!(ad.getType().equals("Int")
              || ad.getType().equals("String")
              || ad.getType().equals("Bool"))) {
            error("Invalid attribute type, must be ‘Int’, ‘Bool’ or ‘String’.",
                  AbsPackage.eINSTANCE.getMTVLAttributeDecl_Type());
        } else if (!(ad.getType().equals("Int"))
                   && ad.isInterval()) {
            error("Can only specify an interval for type ‘Int’.",
                  AbsPackage.eINSTANCE.getMTVLAttributeDecl_Interval());
        } else if (!(ad.getType().equals("Int"))
                   && ad.isSet()) {
            error("Can only specify a value set for type ‘Int’.",
                  AbsPackage.eINSTANCE.getMTVLAttributeDecl_Set());
        }
    }
}
