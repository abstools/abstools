/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models;

import static org.abs_models.ABSTest.Config.EXPECT_PARSE_ERROR;
import static org.abs_models.ABSTest.Config.EXPECT_TYPE_ERROR;
import static org.abs_models.ABSTest.Config.TYPE_CHECK;
import static org.abs_models.ABSTest.Config.WITHOUT_DESUGARING_AFTER_TYPECHECK;
import static org.abs_models.ABSTest.Config.WITHOUT_MODULE_NAME;
import static org.abs_models.ABSTest.Config.WITH_LOC_INF;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.inject.Injector;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.SemanticCondition;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.parser.Main;
import org.abs_models.frontend.parser.XtextToJastAdd;
import org.abs_models.frontend.typechecker.locationtypes.LocationTypeExtension;
import org.abs_models.xtext.AbsStandaloneSetup;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.validation.IResourceValidator;
import org.eclipse.xtext.validation.Issue;

public class ABSTest {
    public static enum Config {
        NONE,
        WITHOUT_MODULE_NAME,
        WITH_LOC_INF,
        EXPECT_PARSE_ERROR,
        EXPECT_TYPE_ERROR,
        EXPECT_WARNING,
        TYPE_CHECK,
        WITHOUT_DESUGARING_AFTER_TYPECHECK
    }

    public static class ABSFileNameFilter implements FilenameFilter {
        @Override
        public boolean accept(File dir, String name) {
            return name.endsWith(".abs");
        }
    }

    private static final File testfileroot = new File("src/test/resources/");

    static protected boolean isSet(Config c, Config...configs) {
        if (configs == null)
            throw new IllegalArgumentException("Must give an array of configs");
        for (Config c2: configs) {
            if (c2 == c)
                return true;
        }
        return false;
    }

    /**
     * Take a file name and returns that name if it points to an existing file,
     * otherwise if {@link File#getAbsoluteFile()} returns a file that points to
     * an existing file, this method returns {@link File#getAbsolutePath()}.
     *
     * @param fileName
     * @return a valid file name from the input file name
     * @throws IllegalArgumentException
     *             if neither the input file name nor
     *             {@link File#getAbsoluteFile()} points to a valid file.
     */
    static protected String resolveFileName(String fileName) {
        File f = new File(fileName);
        if (f.exists()) {
            return fileName;
        }
        f = f.getAbsoluteFile();
        if (f.exists()) {
            return f.getAbsolutePath();
        }
        f = new File(testfileroot, fileName);
        if (f.exists()) { return f.getAbsolutePath(); }
        throw new IllegalArgumentException("File "+fileName+" cannot be read");
    }

    public static Model parseString(String s) throws Exception {
        XtextResourceSet resourceSet = Main.absinjector.getInstance(XtextResourceSet.class);
        // This is a bit gross but seems to work
        ParseHelper<org.abs_models.xtext.abs.CompilationUnit> ph = (ParseHelper<org.abs_models.xtext.abs.CompilationUnit>)(Main.absinjector.getInstance(ParseHelper.class));
        // NOTE: some tests depend on the standard library being first, see
        // e.g. FrontendTest.getLastDecl et al.  Using AST walking methods
        // (Model.lookup() etc.) would be nicer, but rewriting a whole lot of
        // unit tests is a bit time consuming as well.
        resourceSet.createResource(org.eclipse.emf.common.util.URI.createURI(Main.class.getClassLoader().getResource(Main.ABS_STD_LIB).toString()))
            .load(null);
        ph.parse(s, resourceSet);
        boolean hasErrors = false;
        for (Resource r : resourceSet.getResources()) {
            IResourceValidator validator = ((XtextResource)r).getResourceServiceProvider().getResourceValidator();
            java.util.List<Issue> issues = validator.validate(r, org.eclipse.xtext.validation.CheckMode.ALL, CancelIndicator.NullImpl);
            for (Issue issue : issues) {
                if (issue.getSeverity() == Severity.ERROR) hasErrors = true;
            }
        }
        Model result = null;
        if (!hasErrors) {
            result = XtextToJastAdd.fromResourceSet(resourceSet);
        }
        return result;
    }

    protected static Model assertParse(String s, Config... config) {

        String preamble = "module UnitTest; export *; ";
        preamble = preamble + " import * from ABS.StdLib;";
        if (!isSet(WITHOUT_MODULE_NAME, config))
            s = preamble + s;
        try {
            Model p = parseString(s);
            if (isSet(EXPECT_PARSE_ERROR,config)) {
                // in case of syntax errors, p is null
                if (!(p == null) && !p.hasParserErrors()) {
                    fail("Expected to find parse error");
                } else {
                    return p;
                }
            }
            assertNotNull(p);

            if (isSet(WITHOUT_DESUGARING_AFTER_TYPECHECK, config)) {
                p.doAACrewrite = false;
                p.doForEachRewrite = false;
            }

            if (p.hasParserErrors()) {
                fail("Failed to parse: " + s + "\n" + p.getParserErrors().get(0).getMessage());
            } else {
                // make ProductDecl.getProduct() not return null
                p.evaluateAllProductDeclarations();
                if (isSet(TYPE_CHECK, config)) {
                    // copy other choice parts of Main.analyzeFlattenAndRewriteModel
                    p.flattenTraitOnly();
                    p.collapseTraitModifiers();
                    p.expandPartialFunctions();
                    p.expandForeachLoops();
                    p.expandAwaitAsyncCalls();
                    if (isSet(WITH_LOC_INF, config)) {
                        LocationTypeExtension ltie = new LocationTypeExtension(p);
                        p.registerTypeSystemExtension(ltie);
                    }
                    SemanticConditionList l = p.typeCheck();
                    if (isSet(EXPECT_TYPE_ERROR,config)) {
                        if (!l.containsErrors()) {
                            fail("Expected type errors, but none appeared");
                        }
                    } else {
                        if (l.containsErrors()) {
                            fail("Failed to typecheck: " + s + "\n" + l.getFirstError().getMessage());
                        }
                    }
                }
            }
            return p;
        } catch (Exception t) {
            throw new RuntimeException(t); // TODO: remove
        }
    }

    protected Model assertParseError(String absCode) {
        return assertParse(absCode, EXPECT_PARSE_ERROR);
    }

    /**
     * Note: does not handle EXPECT_*.
     * @throws DeltaModellingException
     */
    static public Model assertParseFileOk(String fileName, Config... config) throws IOException,
        WrongProgramArgumentException, InternalBackendException, DeltaModellingException {
        Main main = new Main();
        File file = new File(resolveFileName(fileName));
        Model m;
        if (file.isDirectory()) {
            m = main.parse(Arrays.asList(file.listFiles(f ->
                                                        f.isFile()
                                                        && Main.isABSSourceFile(f))));
        } else {
            m = main.parse(Arrays.asList(file));
        }
        assertNotNull(m);
        m.evaluateAllProductDeclarations();
        return assertParseModelOk(m, config);
    }

    protected Model assertParseFilesOk(Set<String> fileNames, Config... config) throws IOException, InternalBackendException, DeltaModellingException, WrongProgramArgumentException {
        Main main = new Main();
        java.util.List<File> files = fileNames.stream().map(f -> new File(resolveFileName(f))).collect(Collectors.toList());
        Model m = main.parse(files);
        assertNotNull(m);
        return assertParseModelOk(m, config);
    }

    static public Model assertParseModelOk(Model m, Config... config) throws IOException {
        if (m != null) {
            final StringBuffer errs;
            String fileNames = m.getCompilationUnit(0).getFileName();
            for (int i = 1; i < m.getCompilationUnits().getNumChild(); i++)
                fileNames += " & " + m.getCompilationUnit(i).getFileName();

            int parseErrs = m.getParserErrors().size();
            if (parseErrs > 0) {
                errs = new StringBuffer("Parse errors: " + parseErrs + ". First error:\n");
                errs.append(m.getParserErrors().get(0));
                fail("Failed to parse: " + fileNames + "\n" + errs.toString());
                return m;
            }

            int numSemErrs = m.getErrors().getErrorCount();


            errs = new StringBuffer("Semantic errors: " + numSemErrs + "\n");
            if (numSemErrs > 0) {
                for (SemanticCondition error : m.getErrors())
                    errs.append(error.getHelpMessage() + "\n");
                fail("Failed to parse: " + fileNames + "\n" + errs.toString());
            } else if (isSet(TYPE_CHECK, config)) {
                SemanticConditionList l = m.typeCheck();
                if (l.containsErrors()) {
                    for (SemanticCondition error : l)
                        errs.append(error.getHelpMessage() + "\n");
                    fail("Failed to typecheck: " + fileNames + "\n" + errs.toString());
                }
            }
        }
        return m;
    }

}
