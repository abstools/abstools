package abs.backend.java;

import org.junit.Test;

public class InterfaceTests extends JavaBackendTest {
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

    @Test
    public void testVarDeclInterface() {
        assertValid("interface I { } { I x; }");
    }



}
