package abs.frontend.analyser;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Exp;
import abs.frontend.ast.Model;

public class OtherAnalysisTests extends FrontendTest {
    
    @Test
    public void letExp() {
        Model m = assertParseOk("interface I { } class C { { I i = new cog C(); } Unit m() { I i = new cog C(); } } { I i; i = new cog C(); i = new C(); while (true) { i = new cog C(); }}");
        assertEquals(4, m.countNumberOfNewCogExpr());
    }
    

}
