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
        return normalizeNameAgainstDelta(ele, result);
    }

    /**
     * Adapt all our names so that they’re findable by module name, not delta
     * name.  Note that all other ‘qualifiedName’ methods must handle their
     * result in the same way.
     */
    protected QualifiedName qualifiedName(final EObject o) {
        QualifiedName result = computeFullyQualifiedNameFromNameAttribute(o);
        return normalizeNameAgainstDelta(o, result);
    }

    /**
     * Normalize the qualified name.  If o is contained in a delta, change its
     * qname to be prefixed with the delta’s uses clause, or nothing.
     *
     * @param o the object under consideration
     * @param qname the qualified name of o before normalization, or null
     * @return qname if o is not contained in a delta or if qname is null, a normalized name otherwise.
     */
    private static QualifiedName normalizeNameAgainstDelta(EObject o, QualifiedName qname) {
        if (qname == null) return qname;
        DeltaDeclaration d = EcoreUtil2.getContainerOfType(o, DeltaDeclaration.class);
        if (d == null || o.equals(d)) return qname;
        QualifiedName deltaname = QualifiedName.create(d.getName().split("\\."));
        qname = qname.skipFirst(deltaname.getSegmentCount());
        String usedname = d.getUsedModulename();
        if (usedname != null) {
            qname = QualifiedName.create(usedname.split("\\.")).append(qname);
        }
        return qname;
    }

}
