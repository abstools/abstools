package org.abs_models.frontend.typesystem;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.junit.Test;

public class NullCheckerTests extends FrontendTest {

    @Test
    public void varDeclNonNullNoInit() {
        assertTypeErrors("interface I {} { [NonNull] I i; }", ErrorMessage.NON_NULL_VAR_INIT_REQUIRED);
    }

    @Test
    public void varDeclNonNullOK() {
        assertTypeOK("interface I { Unit m(I i); } class C implements I { Unit m([NonNull] I i) { [NonNull] I j = i; } }");
    }
}
