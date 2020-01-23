package org.abs_models.xtext.scoping;

import java.util.List;

import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;
import com.google.inject.Inject;

import org.abs_models.xtext.abs.DeltaDeclaration;
import org.abs_models.xtext.abs.ModuleDeclaration;
import org.abs_models.xtext.abs.ModuleImport;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.scoping.impl.ImportNormalizer;
import org.eclipse.xtext.scoping.impl.ImportedNamespaceAwareLocalScopeProvider;

/**
 * A local scope provider that handles ABS namespace imports.
 *
 * It handles the <code>imports</code> attribute of
 * <code>ModuleDeclaration</code> elements.
 *
 * See
 * https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#scoping
 * for details.
 *
 * @author Rudi Schlatte
 */
public class AbsImportedNamespaceAwareLocalScopeProvider extends ImportedNamespaceAwareLocalScopeProvider {
    // We override this method instead of `getImportedNamespaceResolvers`
    // because we do want to cache the result and the `cache` instance
    // variable is private.
    //
    // We override this method instead of `getImportedNamespace` or
    // `createImportedNamespaceResolver` because `import` statements can
    // import multiple identifiers: the ABS line `import A, B from X;` will
    // result in two `ImportNormalizer` instances.
    //
    // https://www.euclideanspace.com/software/development/eclipse/xtext/infrastructure/importing/index.htm
    // overrides the same method.
    @Override
    protected List<ImportNormalizer> internalGetImportedNamespaceResolvers(final EObject context, final boolean ignoreCase) {

        final List<ImportNormalizer> importedNamespaceResolvers = Lists.newArrayList();
        if (context instanceof ModuleDeclaration) {
            boolean hasStdLibImport = false;
            final ModuleDeclaration moduleDecl = (ModuleDeclaration) context;
            for (final ModuleImport moduleImport : moduleDecl.getImports()) {
                // TODO: check if any import is from ABS.StdLib; add import of that namespace otherwise.
                if (moduleImport.isStar()) {
                    // import * from Modulename;
                    final String name = moduleImport.getModulename();
                    if (name.equals("ABS.StdLib")) hasStdLibImport = true;
                    final ImportNormalizer resolver = createImportedNamespaceResolver(name  + ".*", ignoreCase);
                    if (resolver != null) importedNamespaceResolvers.add(resolver);
                } else if (moduleImport.getModulename() != null) {
                    // import A, B from Modulename;
                    final String name = moduleImport.getModulename();
                    if (name.equals("ABS.StdLib")) hasStdLibImport = true;
                    for (final String id : moduleImport.getIdentifiers()) {
                        final ImportNormalizer resolver = createImportedNamespaceResolver(name + "." + id, ignoreCase);
                        if (resolver != null) importedNamespaceResolvers.add(resolver);
                    }
                } else {
                    // import Modulename.A, Modulename.B;

                    // Nothing to do here: if we created an ImportNormalizer,
                    // it would make Modulename.A available as plain A, which
                    // we do not want, and Xtext already resolves the
                    // qualified name.
                }
                if (!hasStdLibImport) {
                    // We do not override `getImplicitImports` since we only
                    // import all of ABS.StdLib if no part of it has been
                    // explicitly imported.
                    final ImportNormalizer resolver = createImportedNamespaceResolver("ABS.StdLib.*", ignoreCase);
                    if (resolver != null) importedNamespaceResolvers.add(resolver);
                }
            }
        } else if (context instanceof DeltaDeclaration) {
            final DeltaDeclaration deltaDecl = (DeltaDeclaration) context;
            if (deltaDecl.getUsedModulename() != null) {
                // A `uses` clause imports everything from the module, plus
                // inherits all of the module’s import clauses.
                final String name = deltaDecl.getUsedModulename();
                final ImportNormalizer resolver = createImportedNamespaceResolver(name + ".*", ignoreCase);
                if (resolver != null) importedNamespaceResolvers.add(resolver);
                final ResourceSet set = context.eResource().getResourceSet();
                final ModuleDeclaration m = (ModuleDeclaration) Iterators.find(set.getAllContents(),
                                                                               elem -> elem instanceof ModuleDeclaration
                                                                               && ((ModuleDeclaration)elem).getName().equals(name),
                                                                               null);
                if (m != null) {
                    importedNamespaceResolvers.addAll(internalGetImportedNamespaceResolvers(m, ignoreCase));
                } else {
                    // shouldn’t happen, will probably result in errors later
                }
            }
        }
        return importedNamespaceResolvers;
    }

}
