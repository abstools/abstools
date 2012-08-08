/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import static org.junit.Assert.fail;

import java.io.IOException;
import org.junit.Test;
import abs.backend.java.codegeneration.JavaCode;
import abs.backend.java.codegeneration.JavaCodeGenerationException;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class JavaBackendDynamicTest extends JavaBackendTest {
    
    @Override
    protected JavaCode getJavaCode(String absCode, boolean withStdLib) throws Exception {
        Model model = null;
        String code = null;
        code = absCode;
        model = Main.parseString(code, withStdLib);
        if (model.hasErrors()) {
            fail(model.getErrors().get(0).getHelpMessage());
        } else {
            SemanticErrorList el = model.typeCheck();
            if (!el.isEmpty()) {
                fail(el.get(0).getMsg());
            }
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

    @Test
    public void meta1() throws Exception {
        assertValidJavaExecution(true,
                "module Test; import * from ABS.Meta;",
                "interface A {}",
                "class A implements A {}",
                "{",
                    "A obj = new A();",
                    "ObjectMirror m = reflect(obj);",
                    "String name = m.getClassName();",
                    "assert (name == \"A\");",
                "}"
                
        );
    }
}
