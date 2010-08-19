package abs.backend.java;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.PrintWriter;

import junit.framework.Assert;

import abs.ABSTest;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class JavaBackendTest extends ABSTest {
    
    void assertEqual(String absCode, String javaCode) {
        assertEqual(absCode, javaCode,null);
    }
    
    void assertValidStdLib(String absCode) {
        assertValidJava(getJavaCode(absCode, true));
    }
    
    void assertValid(String absCode) {
        assertValidJava(getJavaCode(absCode, false));
    }
    
    void assertValidJava(String javaCode) {
        File tmpFile;
        try {
            tmpFile = getTempFile(javaCode);
            JavaCompiler.compile("-classpath","bin", "-d", "gen/test", tmpFile.getAbsolutePath());
        } catch (Exception e) {
           System.out.println(javaCode);
            Assert.fail(e.getMessage());
        }
    }
    
    String getJavaCode(String absCode, boolean withStdLib) {
        try {
        Model model = null;
        try {
            String code = absCode;
            if (withStdLib) 
                code = "data Unit = Unit; data Bool = True | False; data Int; data String; data Fut<A>; " + code; 
            model = Main.parseString(code, false);
            if (model.hasErrors()) {
                Assert.fail(model.getErrors().get(0).getMsgString());
            } else {
                SemanticErrorList el = model.typeCheck();
                if (!el.isEmpty()) {
                    Assert.fail(el.get(0).getMsg());
                }
            }
            
        } catch (Exception e) {
            Assert.fail(e.getMessage());
            return null;
        }
        
        if (model.hasErrors()) {
            Assert.fail(model.getErrors().getFirst().getMsgString());
            return null;
        }
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        model.generateJava(new PrintStream(out));
        String res = out.toString();
        res = res.replace('\n', ' ');
        res = res.replaceAll("[ ]+", " ");
        res = res.trim();
        return res;
        } catch (NumberFormatException e) {
            Assert.fail(e.getMessage());
            return null;
        }
    }
    
    void assertEqual(String absCode, String javaCode, String pkg) {
        try {
            StringBuffer expectedJavaCode = new StringBuffer();
            if (pkg != null) {
                expectedJavaCode.append("package "+pkg+"; ");
            }
            
            expectedJavaCode.append(JavaBackendConstants.LIB_IMPORT_STATEMENT+" ");
            expectedJavaCode.append(javaCode);
            String generatedJavaCode = getJavaCode(absCode, false);
            Assert.assertEquals(expectedJavaCode.toString(), generatedJavaCode);
            
            assertValidJava(generatedJavaCode);
            
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    private static File getTempFile(String testCode) throws IOException {
        File tmpFile = File.createTempFile("abs", "test");
        PrintWriter p = new PrintWriter(new FileOutputStream(tmpFile));
        p.print(testCode);
        p.close();
        tmpFile.deleteOnExit();
        
        return tmpFile;
    }
    
}
