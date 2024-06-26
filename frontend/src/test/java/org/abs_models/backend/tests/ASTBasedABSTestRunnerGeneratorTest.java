/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.tests;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import org.abs_models.ABSTest;
import org.abs_models.frontend.analyser.SemanticCondition;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.parser.ParserError;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.junit.Test;

/**
 * Unit tests for {@link ASTBasedABSTestRunnerGenerator}
 * @author woner
 *
 */
public class ASTBasedABSTestRunnerGeneratorTest {

    private final static String ABS_UNIT =
                "module AbsUnit; export *;" +
                "[TypeAnnotation] data DataPoint = DataPoint; " +
                "[TypeAnnotation] data Ignored = Ignored;" +
                "[TypeAnnotation] data Test = Test; " +
                "[TypeAnnotation] data Suite = Suite; " +
                "[TypeAnnotation] data Fixture = Fixture; ";

    private final static String TEST_CODE =
                "module Test; export *; import * from AbsUnit;" +
                "[Fixture] interface T { [Test] Unit t(); }" +
                "[Suite] class TI implements T { Unit t() { } }";

    /**
     * NB: type patched to be generic instead of the more specific ModuleDecl because
     * javac is too picky about hamcrests' generics!
     */
    private static class ModuleMatcher<T>
    extends BaseMatcher<T> {

        public boolean matches(Object arg0) {
            if (arg0 instanceof ModuleDecl) {
                ModuleDecl module = (ModuleDecl) arg0;
                if (module.getName().equals(ASTBasedABSTestRunnerGenerator.RUNNER_MAIN)) {
                    return module.hasBlock();
                }
            }
            return false;
        }

        @SuppressWarnings("unused")
        public void describeTo(Description arg0) {
            // TODO Auto-generated method stub

        }
    }

    @SuppressWarnings("unused")
    @Test(expected=IllegalArgumentException.class)
    public final void testABSTestRunnerGeneratorNull() {
        new ASTBasedABSTestRunnerGenerator(null);
    }

    @Test
    public final void testGenerateTestRunner() {
        final Model model;
        try {
            model = ABSTest.parseString(ABS_UNIT + TEST_CODE);
        } catch (Exception e) {
            throw new IllegalStateException("Cannot parse test code",e);
        }

        ABSTestRunnerGenerator generator = new ASTBasedABSTestRunnerGenerator(model);
        ByteArrayOutputStream stream = new ByteArrayOutputStream();
        PrintStream print = new PrintStream(stream);
        generator.generateTestRunner(print);
        String runner = stream.toString();

        try {
            Model result = ABSTest.parseString(ABS_UNIT + TEST_CODE + runner);

            StringBuilder parseErrors = new StringBuilder();
            if (result.hasParserErrors()) {
                parseErrors.append("Syntactic errors: ");
                List<ParserError> es = result.getParserErrors();
                parseErrors.append(es.size());
                parseErrors.append("\n");
                for (ParserError e : es) {
                    parseErrors.append(e.getHelpMessage());
                    parseErrors.append("\n");
                }
            }

            assertFalse("Generated code must not have parse error: "+parseErrors,result.hasParserErrors());

            StringBuilder errors = new StringBuilder();
            if (result.hasErrors()) {
                SemanticConditionList el = result.getErrors();
                errors.append("Semantic errors: ");
                errors.append(el.getErrorCount());
                errors.append("\n");
                for (SemanticCondition error : el) {
                    errors.append(error.getHelpMessage());
                    errors.append("\n");
                }
            }

            assertFalse("Generated code must not have semantic error: "+errors,result.hasErrors());

            result.typeCheck();
            assertFalse("Generated code must not have type error",result.hasTypeErrors());

            assertThat("Has one module that has the name 'AbsUnit.TestRunner' and a main block",
                        result.getModuleDecls(),hasItem(new ModuleMatcher()));

        } catch (Exception e) {
            fail("Cannot throw an exception ");
        }

    }

}
