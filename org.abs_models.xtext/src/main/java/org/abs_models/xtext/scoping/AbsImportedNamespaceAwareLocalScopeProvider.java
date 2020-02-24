package org.abs_models.xtext.scoping;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.common.collect.Iterators;
import com.google.inject.Inject;

import org.abs_models.xtext.abs.DeltaDeclaration;
import org.abs_models.xtext.abs.ModuleDeclaration;
import org.abs_models.xtext.abs.ModuleExport;
import org.abs_models.xtext.abs.ModuleImport;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.impl.ResourceDescriptionsProvider;
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

    @Inject
    private ResourceDescriptionsProvider rdp;

    // We override `internalGetImportedNamespaceResolvers` instead of
    // `getImportedNamespaceResolvers` because we do want to cache the result
    // and the `cache` instance variable of the superclass is private.
    //
    // We override this method instead of `getImportedNamespace` or
    // `createImportedNamespaceResolver` because `import` statements can
    // import multiple identifiers: the ABS line `import A, B from X;` will
    // result in two `ImportNormalizer` instances.
    //
    // https://www.euclideanspace.com/software/development/eclipse/xtext/infrastructure/importing/index.htm
    // and Bettini, “Implementing Domain-Specific Langauges with Xtext and
    // Xtend” (2nd Ed) pg. 279 override the same method.
    @Override
    protected List<ImportNormalizer> internalGetImportedNamespaceResolvers(final EObject context, final boolean ignoreCase) {
        final List<ImportNormalizer> importedNamespaceResolvers
            = super.internalGetImportedNamespaceResolvers(context, ignoreCase);
        if (context instanceof ModuleDeclaration) {
            final ModuleDeclaration moduleDecl = (ModuleDeclaration) context;
            // Find all module declarations -- this happens before linking so
            // we have to do it manually
            final Map<String, ModuleDeclaration> modules = new HashMap<>();
            final IResourceDescriptions index = rdp.getResourceDescriptions(moduleDecl.eResource());
            for (final IEObjectDescription d : index.getExportedObjectsByType(moduleDecl.eClass())) {
                modules.put(d.getQualifiedName().toString(), (ModuleDeclaration) d.getEObjectOrProxy());
            }
            // Handle implicit import of standard library.  We do not override
            // the `getImplicitImports` method since we only import all of
            // ABS.StdLib if no part of it has been explicitly imported.
            final boolean hasStdLibImport = moduleDecl.getImports().stream().anyMatch(i -> i.getModulename().equals("ABS.StdLib"));
            if (!hasStdLibImport) {
                final ImportNormalizer resolver = createImportedNamespaceResolver("ABS.StdLib.*", ignoreCase);
                if (resolver != null) importedNamespaceResolvers.add(resolver);
            }
            for (final ModuleImport moduleImport : moduleDecl.getImports()) {
                if (moduleImport.isStar()) {
                    // import * from Modulename;
                    final String name = moduleImport.getModulename();
                    importAllFromModule(importedNamespaceResolvers, name, modules, ignoreCase);
                } else if (moduleImport.getModulename() != null) {
                    // import A, B from Modulename;
                    final String name = moduleImport.getModulename();
                    importNamesFromModule(importedNamespaceResolvers, name, moduleImport.getIdentifiers(), modules, ignoreCase);
                } else {
                    // import Modulename.A, Modulename.B;

                    // Nothing to do here: if we created an ImportNormalizer,
                    // it would make Modulename.A available as plain A, which
                    // we do not want, and Xtext already resolves the
                    // qualified name.
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
                // TODO import everything from all Deltas that have the same `uses` clause
                if (m != null) {
                    importedNamespaceResolvers.addAll(internalGetImportedNamespaceResolvers(m, ignoreCase));
                } else {
                    // shouldn’t happen, will probably result in errors later
                }
            }
        }
        return importedNamespaceResolvers;
    }

    /**
     * Import all exported symbols from target module, including re-exported
     * symbols.
     *
     * @param resolvers the list of resolvers to add to
     * @param moduleName the name of the module we’re importing from
     * @param ignoreCase whether to ignore case
     */
    private void importAllFromModule(final List<ImportNormalizer> resolvers,
                                     final String moduleName,
                                     final Map<String, ModuleDeclaration> modules,
                                     final boolean ignoreCase)
    {
        // Give up early if module not found -- will be caught during
        // validation
        if (!modules.containsKey(moduleName)) return;
        final ModuleDeclaration module = modules.get(moduleName);
        if (module == null) return;
        for (final ModuleExport export : module.getExports()) {
            if (export.getModulename() != null) {
                if (export.isStar()) {
                    // export * from Mod;
                    for (final ModuleImport imp : module.getImports()) {
                        if (export.getModulename().equals(imp.getModulename())) {
                            if (imp.isStar()) {
                                // export * from Mod; + import * from Mod;
                                importAllFromModule(resolvers, imp.getModulename(), modules, ignoreCase);
                            } else {
                                // export * from Mod; + import A, B, C from Mod;
                                importNamesFromModule(resolvers, imp.getModulename(), imp.getIdentifiers(), modules, ignoreCase);
                            }
                        }
                    }
                } else {
                    // export A, b, C from Mod;
                    // TODO write validator that checks for import of at least A, b, C
                    for (final ModuleImport imp : module.getImports()) {
                        if (export.getModulename().equals(imp.getModulename())) {
                            if (imp.isStar()) {
                                // export A, b, C from Mod; + import * from Mod;
                                importNamesFromModule(resolvers, imp.getModulename(), export.getIdentifiers(), modules, ignoreCase);
                            } else {
                                // export A, b, C from Mod; + import A, e, F from Mod;
                                importNamesFromModule(resolvers,
                                                      imp.getModulename(),
                                                      intersect_ids(export.getIdentifiers(), imp.getIdentifiers()),
                                                      modules, ignoreCase);
                            }
                        }
                    }
                }
            } else {
                if (export.isStar()) {
                    // export *;
                    final ImportNormalizer resolver = createImportedNamespaceResolver(moduleName  + ".*", ignoreCase);
                    if (resolver != null) resolvers.add(resolver);
                } else {
                    // export A, b, C;
                    for (final String id : export.getIdentifiers()) {
                        final ImportNormalizer resolver = createImportedNamespaceResolver(moduleName + "." + id, ignoreCase);
                        if (resolver != null) resolvers.add(resolver);
                    }
                }
            }
        }
    }

    /**
     * Import all symbols from target module that that module actually exports
     * (i.e., the intersection between ‘simpleNames’ and the module exports).
     * This includes re-exported modules.
     *
     * @param resolvers the list of resolvers to add to
     * @param moduleName the name of the module we’re importing from
     * @param simpleNames the list of names to be imported.
     * @param ignoreCase whether to ignore case
     */
    private void importNamesFromModule(final List<ImportNormalizer> resolvers,
                                       final String moduleName,
                                       final List<String> simpleNames,
                                       final Map<String, ModuleDeclaration> modules,
                                       final boolean ignoreCase)
    {
        // Give up early if module not found -- will be caught during
        // validation
        if (!modules.containsKey(moduleName)) return;
        final ModuleDeclaration module = modules.get(moduleName);
        if (module == null) return;

        for (final ModuleExport export : module.getExports()) {
            if (export.getModulename() != null) {
                // Consider importing re-exported names from M -- import
                // intersection of what we want and what is re-exported from M
                if (export.isStar()) {
                    for (final ModuleImport imp : module.getImports()) {
                        if (export.getModulename().equals(imp.getModulename())) {
                            if (imp.isStar()) {
                                importNamesFromModule(resolvers,
                                                      imp.getModulename(),
                                                      simpleNames,
                                                      modules, ignoreCase);
                            } else {
                                importNamesFromModule(resolvers,
                                                      imp.getModulename(),
                                                      intersect_ids(simpleNames, imp.getIdentifiers()),
                                                      modules, ignoreCase);
                            }
                        }
                    }
                } else {
                    final List<String> simpleNames2 = intersect_ids(simpleNames, export.getIdentifiers());
                    if (!simpleNames.isEmpty()) {
                        for (final ModuleImport imp : module.getImports()) {
                            if (export.getModulename().equals(imp.getModulename())) {
                                if (imp.isStar()) {
                                    importNamesFromModule(resolvers,
                                                          imp.getModulename(),
                                                          simpleNames2,
                                                          modules,
                                                          ignoreCase);
                                } else {
                                    // Double subtraction:
                                    // - We wanted A,b,C from M1 == simpleNames
                                    // - M1 exports A,b from M2  == simpleNames2
                                    // - M1 imports A from M2
                                    // (This case should probably be caught by
                                    // the validator, but let’s be defensive.)
                                    importNamesFromModule(resolvers,
                                                          imp.getModulename(),
                                                          intersect_ids(simpleNames2, imp.getIdentifiers()),
                                                          modules, ignoreCase);
                                }
                            }
                        }
                    }
                }
            } else {
                if (export.isStar()) {
                    // export *;
                    for (final String id : simpleNames) {
                        final ImportNormalizer resolver = createImportedNamespaceResolver(moduleName + "." + id, ignoreCase);
                        if (resolver != null) resolvers.add(resolver);
                    }
                } else {
                    for (final String id : intersect_ids(simpleNames, export.getIdentifiers())) {
                        final ImportNormalizer resolver = createImportedNamespaceResolver(moduleName + "." + id, ignoreCase);
                        if (resolver != null) resolvers.add(resolver);
                    }
                }
            }
        }
    }

    /**
     * Return a fresh List containing the intersection of the elements of l1
     * and l2
     */
    private List<String> intersect_ids(final List<String> l1, final List<String> l2) {
        // KLUDGE: this set operation is expensive, but the number of inputs
        // is small
        return l1.stream()
            .filter(s -> l2.contains(s))
            .collect(Collectors.toList());
    }
}
