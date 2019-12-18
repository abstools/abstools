package org.abs_models.xtext.tests;

import com.google.inject.Inject;
import org.abs_models.xtext.abs.CompilationUnit;
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
    public void parseEmptyModule() {
        parseErrorFreeString("module Xtext;");
    }


    /**
     * Parse a string and return the xtext CompilationUnit.
     * @param model an ABS model as a string
     * @return the parsed model
     */
    private CompilationUnit parseString(final String model) {
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
     * contain a valid ABS model.
     * @param model  an ABS model as a string
     * @return the parsed model
     */
    private CompilationUnit parseErrorFreeString(final String model) {
        final CompilationUnit result = parseString(model);
        Assertions.assertNotNull(result);
        final EList<Resource.Diagnostic> errors = result.eResource().getErrors();
        Assertions.assertTrue(errors.isEmpty(),
                              "Unexpected errors: "
                              + IterableExtensions.join(errors, ", "));
        return result;
    }

}
