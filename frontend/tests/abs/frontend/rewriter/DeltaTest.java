package abs.frontend.rewriter;

import java.util.Collection;

import org.junit.Test;
import static org.junit.Assert.*;

import abs.frontend.FrontendTest;
import abs.frontend.delta.Delta;
import abs.frontend.ast.*;

public class DeltaTest extends FrontendTest {

    @Test
    public void AddGetClassModifiers() {
        Delta delta = new Delta("MyModule.MyDelta");
        ClassModifier cm = new AddClassModifier(new ClassDecl());
        delta.addClassModifier("MyModule", cm);
        assertTrue(delta.appliesTo("MyModule"));
        assertFalse(delta.appliesTo("MyOtherModule"));

        Collection<ClassModifier> cms1 = delta.getClassModifiers("MyModule");
        assertTrue(cms1.contains(cm));

        Collection<ClassModifier> cms2 = delta.getClassModifiers("MyOtherModule");
        assertTrue(cms2.isEmpty());
    }
}
