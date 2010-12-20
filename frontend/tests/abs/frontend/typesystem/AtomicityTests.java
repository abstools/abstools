package abs.frontend.typesystem;

import org.junit.Test;

import abs.frontend.FrontendTest;

public class AtomicityTests extends FrontendTest {

    @Test
    public void awaitStmt() {
        checkStmt("await True");
    }
    
    @Test
    public void suspendStmt() {
        checkStmt("suspend");
    }

    @Test
    public void syncCallStmt() {
        checkStmt("this.n(); this.atomN()");
    }

    @Test
    public void getExp() {
        checkStmt("Fut<Unit> f; f.get;");
    }
    
    public void checkStmt(String s) {
        assertTypeErrors("class C { [Atomic] Unit m() { "+s+"; } Unit n() { } [Atom] Unit atomN() { }}");
        assertTypeOK("class C { Unit m() { "+s+"; } Unit n() { } Unit atomN() { }}");
        
    }
    
}
