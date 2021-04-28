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
    public void register(final EValidatorRegistrar registrar) {
        // nothing to do
    }

    // Deltas
    @Check public void checkAllowedAddedDeclaration(final DeltaModuleModifier mod) {
        if (mod.getAddedDeclaration() != null) {
            final Declaration d = mod.getAddedDeclaration();
            if (!(d.getClassDeclaration() != null
                  || d.getInterfaceDeclaration() != null
                  || d.getFunctionDeclaration() != null
                  || d.getDatatypeDeclaration() != null
                  || d.getTypesynonymDeclaration() != null
                  )) {
                error("Trying to add an unsupported type of declaration",
                      AbsPackage.eINSTANCE.getDeltaModuleModifier_AddedDeclaration());
            }
        }
    }

    @Check public void checkNoFieldInitializerForDeltaCondition(final DeltaCondition d) {
        if (d.getDeltaFieldCondition() != null
            && d.getDeltaFieldCondition().isHasInit()) {
            error("Cannot use a field initializer here",
                  AbsPackage.eINSTANCE.getDeltaCondition_DeltaFieldCondition());
        }
    }

    @Check public void checkNoFieldInitializerForRemovedField(final ClassModifier cm) {
        if (cm.getRemovedField() != null
            && cm.getRemovedField().isHasInit()) {
            error("Cannot use a field initializer here",
                  AbsPackage.eINSTANCE.getClassModifier_RemovedField());
        }
    }

    @Check public void checkOnlyTrueOrFalseDeltaParameter(final AttributeAssignmentValue_Bool aavb) {
        if (!(aavb.getValue().equals("True") || aavb.getValue().equals("False"))) {
            error("Invalid Boolean value, must be ‘True’ or ‘False’.",
                  AbsPackage.eINSTANCE.getAttributeAssignmentValue_Bool_Value());
        }
    }

    @Check public void checkFeatureAttributeParameterType(final MTVLAttributeDeclaration ad) {
        if (!(ad.getType().equals("Int")
              || ad.getType().equals("String")
              || ad.getType().equals("Bool"))) {
            error("Invalid attribute type, must be ‘Int’, ‘Bool’ or ‘String’.",
                  AbsPackage.eINSTANCE.getMTVLAttributeDeclaration_Type());
        } else if (!(ad.getType().equals("Int"))
                   && ad.isInterval()) {
            error("Can only specify an interval for type ‘Int’.",
                  AbsPackage.eINSTANCE.getMTVLAttributeDeclaration_Interval());
        } else if (!(ad.getType().equals("Int"))
                   && ad.isSet()) {
            error("Can only specify a value set for type ‘Int’.",
                  AbsPackage.eINSTANCE.getMTVLAttributeDeclaration_Set());
        }
    }
}
