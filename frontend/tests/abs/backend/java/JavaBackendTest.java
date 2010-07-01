package abs.backend.java;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.PrintWriter;

import org.junit.Test;

import junit.framework.Assert;

import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class JavaBackendTest {
    private static File getTempFile(String testCode) throws IOException {
        File tmpFile = File.createTempFile("abs", "test");
        PrintWriter p = new PrintWriter(new FileOutputStream(tmpFile));
        p.print(testCode);
        p.close();
        tmpFile.deleteOnExit();
        
        return tmpFile;
    }

    private static String testCode() {
        return "interface A { }\n" +
        	   "class B implements A { } \n" +
        	   "data Void { }\n";
    }
    
    @Test
    public void testEmptyClass() {
        assertEqual("class A { }", "class A { }");
    }
    
    @Test
    public void testEmptyInterface() {
        assertEqual("interface A { }", "interface A { }");
    }

    void assertEqual(String absCode, String javaCode) {
        try {
            InputStream in = getInputStream(absCode);
            Model model = Main.parse(in);
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            model.generateJava(new PrintStream(out));
            String res = out.toString();
            res = res.replace('\n', ' ');
            res = res.replaceAll("  ", " ");
            res = res.trim();
            Assert.assertEquals(javaCode, res);
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    private InputStream getInputStream(String absCode) {
        return new ByteArrayInputStream(absCode.getBytes());
    }
}
