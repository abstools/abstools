/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.absunit;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import abs.backend.java.codegeneration.JavaCode;
import abs.backend.java.codegeneration.JavaCodeGenerationException;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Decl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.parser.Main;

public class ABSUnitRunner extends Main {

    private String destDir = "/Users/bubel/tmp/abs/bin";
    private String sourceDir = "/Users/bubel/tmp/abs/java";
    private String modelDir = "/Users/bubel/tmp/abs/model/Assertions.abs";

    private List<MethodSig> testMethods = new ArrayList<>(10);

    public void compile() throws Exception {
        final Model model = parseFiles(modelDir);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            return;
        compile(model, new File(destDir));

        analyzeModelUnit(model);
    }

    private void analyzeModelUnit(Model model) {
        System.out.println("Analyzing model:");

        for (CompilationUnit cu : model.getCompilationUnits()) {
            System.out.println(cu.getFileName());

            for (ModuleDecl m : cu.getModuleDecls()) {
                for (Decl cd : m.getDecls()) {
                    if (cd instanceof ClassDecl) {
                        for (MethodSig ms : ((ClassDecl)cd).getAllMethodSigs()) {
                            for (Annotation a : ms.getAnnotations()) {
                                if (a.getType().getSimpleName().equals("Test")) {
                                    System.out.println("Found test method:" + ms.getName());
                                    testMethods.add(ms);
                                } else if (a.getType().getSimpleName().equals("Suite")) {
                                    System.out.println("Found test suite:" + ms.getName());
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    private void compile(Model m, File destDir) throws IOException, JavaCodeGenerationException {
        JavaCode javaCode = new JavaCode(destDir);
        m.generateJavaCode(javaCode, true);
        javaCode.compile();
    }

    /**
     * registers the ABS Unit observers and runs the test suite
     *
     * @throws IOException
     * @throws JavaCodeGenerationException
     */
    private void runTestSuite() throws IOException, JavaCodeGenerationException {
    }


    public static void main(String... args) {
        try {
            new ABSUnitRunner().compile();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }




}
