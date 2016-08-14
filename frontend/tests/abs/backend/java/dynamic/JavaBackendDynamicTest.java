/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.dynamic;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Scanner;

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
    protected JavaCode getJavaCode(String absCode, Config... config) throws Exception {
        Model model = null;
        String code = null;
        code = absCode;
        /* TODO: why not parse the ABSTest way -- sooner or later a flag will fall over... [stolz]*/
        model = Main.parseString(code, isSet(Config.WITH_STD_LIB, config));
        if (model.hasErrors()) {
            fail(model.getErrors().getFirstError().getHelpMessage());
        } else {
            // Omit type checking for now as it can hinder dynamic program evolution
            // TODO infer types...
//          SemanticConditionList el = model.typeCheck();
//            if (el.containsErrors()) {
//                fail(el.get(0).getMsg());
//            }
        }

        if (model.hasErrors()) {
            fail(model.getErrors().getFirstError().getHelpMessage());
            return null;
        }
        return getJavaCodeDynamic(model);
    }

    static JavaCode getJavaCodeDynamic(Model model) throws IOException, JavaCodeGenerationException {
        JavaCode code = new JavaCode();
        model.generateJavaCodeDynamic(code);
        return code;
    }

    protected String readAbsFile(String fileName) throws FileNotFoundException {
        final Scanner s;
        String buffer = (s = new Scanner(new File(fileName))).useDelimiter("\\Z").next();
        s.close();
        return buffer;
    }
    
}
