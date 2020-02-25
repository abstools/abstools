package org.abs_models.xtext.scoping;

import com.google.inject.Singleton;

import org.abs_models.xtext.abs.Expression;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.EObjectDescription;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.impl.DefaultResourceDescriptionStrategy;
import org.eclipse.xtext.util.IAcceptor;

@Singleton
public class AbsResourceDescriptionStrategy extends DefaultResourceDescriptionStrategy {

    @Override
    public boolean createEObjectDescriptions(EObject eObject, IAcceptor<IEObjectDescription> acceptor) {
        // Handle various nodes specially here to keep their contents from
        // being indexed (see Bettini pg. 281ff.)
        boolean result = super.createEObjectDescriptions(eObject, acceptor);
        return result;
    }
}
