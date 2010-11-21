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

    @Test
    public void getExpr() {
        assertValidStdLib("{ Fut<String> fu; fu.get; }");
    }

    @Test
    public void useofJavaKeywordsVariable() {
        assertValidStdLib("{ Bool continue = False; }");
    }

    @Test
    public void useofJavaKeywordsField() {
        assertValidStdLib("class C(Bool continue) { Unit m() { Bool goto = continue; }}");
    }

    @Test
    public void useofJavaKeywordsMethod() {
        assertValidStdLib("class C() { Unit continue() { this.continue(); }}");
    }

    @Test
    public void useofJavaKeywordsMethodInterface() {
        assertValidStdLib("interface I { Unit continue(); } { I i; i.continue(); }");
    }

    @Test
    public void useofJavaKeywordsFunction() {
        assertValidStdLib("def Unit continue() = Unit; { Unit u = continue(); }");
    }

    @Test
    public void useofJavaKeywordsPattern() {
        assertValidStdLib("def Bool continue(Bool break) = case break { false => break; };");
    }

    @Test
    public void useOfVariablesInsideCase() {
        assertValidStdLib("{ Bool b = True; Bool c = case b { _ => b; }; }");
    }

    @Test
    public void useOfVariablesInsideLet() {
        assertValidStdLib("{ Bool b = True; Bool c = let (Bool x) = True in b; }");
    }

}
