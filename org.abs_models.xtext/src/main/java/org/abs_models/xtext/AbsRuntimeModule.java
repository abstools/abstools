/*
 * generated by Xtext 2.18.0
 */
package org.abs_models.xtext;

import com.google.inject.Binder;
import com.google.inject.name.Names;

import org.eclipse.xtext.scoping.impl.ImportedNamespaceAwareLocalScopeProvider;

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
public class AbsRuntimeModule extends AbstractAbsRuntimeModule {
    // See https://www.euclideanspace.com/software/development/eclipse/xtext/infrastructure/importing/index.htm
    @Override
    public void configureIScopeProviderDelegate(Binder binder) {
        // TODO: implement subclass of
        // ImportedNamespaceAwareLocalScopeProvider, override
        // its getImplicitImports et al.
        binder.bind(org.eclipse.xtext.scoping.IScopeProvider.class).annotatedWith(Names.named(org.eclipse.xtext.scoping.impl.AbstractDeclarativeScopeProvider.NAMED_DELEGATE)).to(ImportedNamespaceAwareLocalScopeProvider.class);
    }
}