package abs.backend.java;

import org.junit.Test;

public class JavaExprTests extends JavaBackendTest {

    @Test
    public void caseTrue() {
        assertValidStdLib("def Bool f(Bool x) = case x { True => True; False => False; }; ");
    }
    
    @Test
    public void casePatternVar() {
        assertValidStdLib("data Foo = Bar(Bool); def Bool f(Foo x) = case x { Bar(y) => y; }; ");
    }

    @Test
    public void appendright() {
        assertValidStdLib("def List<A> appendrightTest<A>(List<A> list, A p) = concatenate(list, Cons(p, Nil));");
    }
    
}
