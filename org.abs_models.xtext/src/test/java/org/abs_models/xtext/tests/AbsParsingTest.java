package org.abs_models.xtext.tests;

import com.google.inject.Inject;
import org.abs_models.xtext.abs.CompilationUnit;
import org.abs_models.xtext.abs.ExpressionStatement;
import org.abs_models.xtext.abs.ModuleDeclaration;
import org.abs_models.xtext.abs.NewExpression;
import org.abs_models.xtext.tests.AbsInjectorProvider;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.extensions.InjectionExtension;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(InjectionExtension.class)
@InjectWith(AbsInjectorProvider.class)
@SuppressWarnings("all")
public class AbsParsingTest {
    @Inject
    private ParseHelper<CompilationUnit> parseHelper;

    @Test
    public void linkClass() {
        CompilationUnit result = parseString(
                                             "module Decl;"
                                             + "export *;"
                                             + "class C { }"
                                             + "module Main;"
                                             + "import * from Decl;"
                                             + "{ new C(); }"
                                             );
        ModuleDeclaration m = result.getModules().get(1);
        
        Assertions.assertEquals("Main", m.getName());
        NewExpression e = (NewExpression)((ExpressionStatement)m.getMainblockStatements().get(0)).getExpression();
        Assertions.assertNotNull(e.getClassname());
    }

    @Test
    public void parseEmptyModule() {
        CompilationUnit result = parseString("module Xtext;");
        Assertions.assertEquals("Xtext", result.getModules().get(0).getName());
    }


    /**
     * Parse a string and return the xtext CompilationUnit.  Does not check
     * for syntax errors etc.
     * @param model an ABS model as a string
     * @return the parsed model
     */
    private CompilationUnit parseStringRaw(final String model) {
        CompilationUnit result = null;
        try {
            result = this.parseHelper.parse(model);
        } catch (final Exception e) {
            throw Exceptions.sneakyThrow(e);
        }
        Assertions.assertNotNull(result);
        return result;
    }

    /**
     * Parse a string and return the xtext CompilationUnit.  The result will
     * contain a syntactically valid ABS model.
     * @param model  an ABS model as a string
     * @return the parsed model
     */
    private CompilationUnit parseString(final String model) {
        final CompilationUnit result = parseStringRaw(model);
        Assertions.assertNotNull(result);
        final EList<Resource.Diagnostic> errors = result.eResource().getErrors();
        Assertions.assertTrue(errors.isEmpty(),
                              "Unexpected errors: "
                              + IterableExtensions.join(errors, ", "));
        return result;
    }

}
