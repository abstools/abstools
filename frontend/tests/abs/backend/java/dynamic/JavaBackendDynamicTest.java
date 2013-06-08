/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.dynamic;

import static abs.ABSTest.Config.TYPE_CHECK;
import static abs.ABSTest.Config.WITH_STD_LIB;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Scanner;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import abs.backend.java.JavaBackendTest;
import abs.backend.java.codegeneration.JavaCode;
import abs.backend.java.codegeneration.JavaCodeGenerationException;
import abs.backend.java.lib.runtime.ABSDynamicRuntime;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class JavaBackendDynamicTest extends JavaBackendTest {
    
    public JavaBackendDynamicTest() {
        super();
        absArgs.add("-dynamic");
    }
    
    @Override
    protected ABSRuntime makeAbsRuntime() {
        return new ABSDynamicRuntime();
    }

    @Override
    protected JavaCode getJavaCode(String absCode, boolean withStdLib) throws Exception {
        Model model = null;
        String code = null;
        code = absCode;
        model = Main.parseString(code, withStdLib);
        if (model.hasErrors()) {
            fail(model.getErrors().get(0).getHelpMessage());
        } else {
            // Omit type checking for now as it can hinder dynamic program evolution
            // TODO infer types...
//          SemanticErrorList el = model.typeCheck();
//            if (!el.isEmpty()) {
//                fail(el.get(0).getMsg());
//            }
        }

        if (model.hasErrors()) {
            fail(model.getErrors().getFirst().getHelpMessage());
            return null;
        }
        return getJavaCodeDynamic(model);
    }

    static JavaCode getJavaCodeDynamic(Model model) throws IOException, JavaCodeGenerationException {
        JavaCode code = new JavaCode();
        model.generateJavaCodeDynamic(code);
        return code;
    }

    @Override
    protected void assertValidJavaFile(String absFile, boolean useStdLib) throws Exception {
        Model m = assertParseFileOk(absFile, WITH_STD_LIB, TYPE_CHECK);
        assertValidJava(getJavaCodeDynamic(m));
    }

    protected String readAbsFile(String fileName) throws FileNotFoundException {
        String buffer = new Scanner(new File(fileName)).useDelimiter("\\Z").next();
        return buffer;
    }
    
}
