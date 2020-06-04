package org.abs_models.frontend.typesystem;

import static org.junit.Assert.assertEquals;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticCondition;
import org.abs_models.frontend.analyser.SemanticWarning;
import org.junit.Test;

public class NullCheckerTests extends FrontendTest {

    @Test
    public void varDeclNonNullNoInit() {
        assertTypeErrors("interface I {} { [NonNull] I i; }", ErrorMessage.NULLABLE_TYPE_MISMATCH);
    }

    @Test
    public void varDeclNonNullNull() {
        assertTypeErrors("interface I {} { [NonNull] I i = null; }", ErrorMessage.NULLABLE_TYPE_MISMATCH);
    }

    @Test
    public void varDeclNonNullOK() {
        assertTypeOK("interface I { Unit m([NonNull] I i); } class C implements I { Unit m([NonNull] I i) { [NonNull] I j = i; } }");
    }

    @Test
    public void testMethodWrongRet() {
        assertTypeErrors("interface I {} class C implements I { [NonNull] I m(I i) { return i; } }", ErrorMessage.NULLABLE_TYPE_MISMATCH);
    }

    @Test
    public void testIfCondition1() {
        assertWarnings("interface I {} class C implements I { Unit m([NonNull] I i) { if (i != null) skip; } }");
    }

    @Test
    public void testIfCondition2() {
        assertWarnings("interface I {} class C implements I { Unit m([NonNull] I i) { if (i == null) skip; } }");
    }

    @Test
    public void testIfCondition3() {
        assertWarnings("interface I {} class C implements I { Unit m(I i) { i = null; if (i != null) skip; } }");
    }

    @Test
    public void testIfCondition4() {
        assertWarnings("interface I {} class C implements I { Unit m(I i) { i = null; if (i == null) skip; } }");
    }

    @Test
    public void onlyAnnotateCorrectType1() {
        assertTypeErrors("interface I { [NonNull] Unit m(); }", ErrorMessage.NULLABLE_TYPE_ONLY_REF_OR_FUT);
    }

    @Test
    public void onlyAnnotateCorrectType2() {
        assertTypeErrors("interface I { Unit m([NonNull] Int n); }", ErrorMessage.NULLABLE_TYPE_ONLY_REF_OR_FUT);
    }

    @Test
    public void onlyAnnotateCorrectType3() {
        assertTypeErrors("{ [NonNull] Int n = 1; }", ErrorMessage.NULLABLE_TYPE_ONLY_REF_OR_FUT);
    }

    @Test
    public void wrongArgTypes() {
        assertTypeErrors("interface I { Unit m([NonNull] I i1, [NonNull] I i2); } class C { Unit m([NonNull] I i1, [NonNull] I i2) { I j; if (4 == 3) j = i2; i1.m(null, j); } }", ErrorMessage.NULLABLE_TYPE_MISMATCH);
    }

    @Test
    public void wrongArgTypesFn() {
        assertTypeErrors("def [NonNull] I orElse(I i1, [NonNull] I i2) = when i1 == null then i2 else i1; interface I { } class C implements I { Unit m(I i) { orElse(this, i); } }", ErrorMessage.NULLABLE_TYPE_MISMATCH);
    }

    @Test
    public void assignNull() {
        assertTypeErrors("interface I {} class C { Unit m([NonNull] I i) { i = null; } }", ErrorMessage.NULLABLE_TYPE_MISMATCH);
    }

    @Test
    public void onlyTemp() {
        assertTypeOK("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i = new C(); [NonNull] I j = i; i = null; } }");
    }

    @Test
    public void overrideWrongParam() {
        assertTypeErrors("interface I { Unit m(I i); } class C implements I { Unit m([NonNull] I i) { } }", ErrorMessage.NULLABLE_TYPE_MISMATCH);
    }

    @Test
    public void overrideWrongRet() {
        assertTypeErrors("interface I { [NonNull] I m(I i); } class C implements I { I m(I i) { return new C(); } }", ErrorMessage.NULLABLE_TYPE_MISMATCH);
    }

    @Test
    public void overrideOKParam1() {
        assertTypeOK("interface I { Unit m([NonNull] I i); } class C implements I { Unit m([NonNull] I i) { } }");
    }

    @Test
    public void overrideOKParam2() {
        assertTypeOK("interface I { Unit m([NonNull] I i); } class C implements I { Unit m(I i) { } }");
    }

    @Test
    public void overrideOKRet1() {
        assertTypeOK("interface I { [NonNull] I m(I i); } class C implements I { [NonNull] I m(I i) { return new C(); } }");
    }

    @Test
    public void overrideOKRet2() {
        assertTypeOK("interface I { I m(I i); } class C implements I { [NonNull] I m(I i) { return new C(); } }");
    }
}
