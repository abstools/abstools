package abs.backend.java;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

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
        try {
            InputStream in = getInputStream(absCode);
            Model model = Main.parse(in);
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            model.generateJava(new PrintStream(out));
            String res = out.toString();
            res = res.replace('\n', ' ');
            res = res.replaceAll("[ ]+", " ");
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
