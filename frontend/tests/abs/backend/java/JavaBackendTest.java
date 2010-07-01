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

import org.junit.Test;

import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class JavaBackendTest {
    
    @Test
    public void testEmptyClass() {
        assertEqual("class A { }", "class A implements ABSClassType { }");
    }
    
    @Test
    public void testEmptyInterface() {
        assertEqual("interface A { }", "interface A extends ABSInterfaceType { }");
    }
    
    @Test
    public void testInterfaceExtend() {
        assertEqual("interface A { } interface B extends A { }", 
                "interface A extends ABSInterfaceType { } "+
                "interface B extends ABSInterfaceType, A { }");
    }

    @Test
    public void testClassOneInterface() {
        assertEqual("interface I { } class C implements I { }", 
                    "interface I extends ABSInterfaceType { } "+
                    "class C implements ABSClassType, I { }");
    }

    void assertEqual(String absCode, String javaCode) {
        assertEqual(absCode, javaCode,null);
    }
    
    void assertEqual(String absCode, String javaCode, String pkg) {
        try {
            InputStream in = getInputStream(absCode);
            Model model = Main.parse(in);
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            model.generateJava(new PrintStream(out));
            String res = out.toString();
            res = res.replace('\n', ' ');
            res = res.replaceAll("[ ]+", " ");
            res = res.trim();
            
            StringBuffer expectedJavaCode = new StringBuffer();
            if (pkg != null) {
                expectedJavaCode.append("package "+pkg+"; ");
            }
            
            expectedJavaCode.append(JavaBackendConstants.LIB_IMPORT_STATEMENT+" ");
            expectedJavaCode.append(javaCode);
            
            Assert.assertEquals(expectedJavaCode.toString(), res);
            
            File tmpFile = getTempFile(out.toString());
            JavaCompiler.compile("-classpath","bin", "-d", "gen/test", tmpFile.getAbsolutePath());
            
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    private InputStream getInputStream(String absCode) {
        return new ByteArrayInputStream(absCode.getBytes());
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
