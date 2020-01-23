package org.abs_models.xtext.tests;

import java.io.File;
import java.nio.file.Files;

import com.google.inject.Inject;

import org.abs_models.xtext.abs.CompilationUnit;
import org.abs_models.xtext.abs.ExpressionStatement;
import org.abs_models.xtext.abs.ModuleDeclaration;
import org.abs_models.xtext.abs.NewExpression;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.extensions.InjectionExtension;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.testing.util.ResourceHelper;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
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

    @Inject
    private ValidationTestHelper validationTestHelper;

    @Inject
    private ResourceSet resourceSet;

    @Inject
    private ResourceHelper resourceHelper;

    @Test
    public void parseCaseStudy() {
        parseFiles("examples/chatPL/Chat.abs", "examples/chatPL/ChatPL.abs", "examples/chatPL/FeatureModel.abs", "examples/chatPL/Management.abs", "examples/chatPL/Products.abs");
    }




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
        Assertions.assertNotNull(e.getClass());
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

    /**
     * Parse a set of files.  The result is accessible in the `resourceSet`
     * field.  Does not check for syntax errors etc.
     *
     * File names should be absolute, relative to this project’s
     * directory, relative to the `frontend/` sibling project, or relative to
     * the `src/test/resources/` path of this project.
     * @param files list of files to parse
     */
    private void parseFilesRaw(String... files) {
        for (String name : files) {
            File file = new File(name);
            if (!file.exists()) file = new File("../frontend/", name);
            if (!file.exists()) file = file.getAbsoluteFile();
            if (!file.exists()) file = new File("src/test/resources/", name);
            try {
                parseHelper.parse(Files.newInputStream(file.toPath()),
                                  URI.createFileURI(file.getAbsolutePath()), null, resourceSet);
            } catch (final Exception e) {
                throw Exceptions.sneakyThrow(e);
            }
        }
    }

    /**
     * Parse a set of files.  The result is accessible in the `resourceSet`
     * field.
     *
     * File names should be absolute, relative to this project’s
     * directory, relative to the `frontend/` sibling project, or relative to
     * the `src/test/resources/` path of this project.
     * @param files list of names of files to parse
     */
    private void parseFiles(String... files) {
        parseFilesRaw(files);
        for (Resource r : resourceSet.getResources()) {
            validationTestHelper.assertNoErrors(r);
        }
    }
}
