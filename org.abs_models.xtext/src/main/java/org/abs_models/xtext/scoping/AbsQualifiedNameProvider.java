package org.abs_models.xtext.scoping;

import java.util.List;

import org.abs_models.xtext.abs.DataConstructorDeclaration;
import org.abs_models.xtext.abs.DeltaDeclaration;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.naming.DefaultDeclarativeQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;

public class AbsQualifiedNameProvider extends DefaultDeclarativeQualifiedNameProvider {

    /**
     * Do not use name of datatype in name of constructor.
     * I.e., for:
     *   module M;
     *   data T = C1 ;
     * the qualified name of ‘C1’ is ‘M.C1’ not ‘M.T.C1’.
     */
    protected QualifiedName qualifiedName(final DataConstructorDeclaration ele){
        QualifiedName result = computeFullyQualifiedNameFromNameAttribute(ele);
        if (result != null) {
            if (result.getSegmentCount() >= 2) {
                result = result.skipLast(2).append(result.getLastSegment());
            }
        }
        return result;
    }

    /**
     * Use the name of the module in the ‘uses’ clause, or null if no module
     * used.  (Unqualified declarations in a delta are added to its used
     * module.)
     */
    protected QualifiedName qualifiedName(final DeltaDeclaration d) {
        if (d.getUsedModulename() != null) {
            return QualifiedName.create(d.getUsedModulename().split("\\."));
        } else {
            return null;
        }
    }

}
