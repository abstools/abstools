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
     * Adapt all our names so that they’re findable by module name, not delta
     * name.  Note that all other ‘qualifiedName’ methods must handle their
     * result in the same way.
     */
    protected QualifiedName qualifiedName(final DeltaDeclaration d) {
        // KLUDGE: this makes the index automatically work for all and lookups
        // inside this delta, but means the delta itself will not get added to
        // the index or added with the same name as its module.  To fix this,
        // add a method `computeFullyQualifiedNameFromNameAttribute(EObject)`
        // and replace the prefix of qualified names instead.  (We cannot do
        // this in `qualifiedName(EObject)` since that method gets called
        // recursively.)
        //
        // See `EcoreUtil2.getParentOfType` to check whether we are in a delta.
        if (d.getUsedModulename() != null) {
            return QualifiedName.create(d.getUsedModulename().split("\\."));
        } else {
            return null;
        }
    }

}
