package abs.frontend.parser;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.Model;

public class RecoverTest extends FrontendTest {
    

    @Test
    public void declTest() {
        Model m = assertParseError(" class C { class D { }");
        assertContainsDeclWithName(m,"D");
    }

    @Test
    public void intfTest() {
        Model m = assertParseError(" class I { ");
        assertContainsDeclWithName(m,"I");
    }
    
    private void assertContainsDeclWithName(Model m, String name) {
        boolean found = false;
        for (Decl d : m.getCompilationUnit(0).getModuleDecl(0).getDecls()) {
            if (d.getName().equals(name)) {
                found = true;
            }
        }
        assertTrue("Did not found decl with name "+name,found);
    }
}
