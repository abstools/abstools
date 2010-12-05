package abs.frontend.typesystem;

import org.junit.Test;

import abs.frontend.FrontendTest;

public class ClassKindTests extends FrontendTest {

    @Test
    public void cogKind() {
        assertTypeErrors("interface I {} [COG] class C implements I {} { I i; i = new C(); }");
        assertNoTypeErrors("interface I {} [COG] class C implements I {} { I i; i = new cog C(); }");
    }
    
    @Test
    public void plainKind() {
        assertNoTypeErrors("interface I {} [Plain] class C implements I {} { I i; i = new C(); }");
        assertTypeErrors("interface I {} [Plain] class C implements I {} { I i; i = new cog C(); }");
    }

    
}
