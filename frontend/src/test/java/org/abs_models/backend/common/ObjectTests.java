/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import java.io.File;

import org.abs_models.backend.BackendTestDriver;
import org.junit.Assume;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class ObjectTests extends SemanticTests {

    public ObjectTests(BackendTestDriver d) {
        super(d);
    }

    @Test
    public void nullCompare() throws Exception {
        assertEvalTrue("interface I {} { Bool testresult = False; I i = null; if (i == null) testresult = True; }");
    }

    @Test
    public void nullCompare2() throws Exception {
        assertEvalTrue("""
            interface I {}
            class C implements I {}
            {
                I o = new C();
                Bool testresult = o > null;
            }
            """);
    }

    @Test
    public void refEq() throws Exception {
        assertEvalTrue("interface I {} class C implements I {} { I i1 = new local C(); Bool testresult = i1 == i1; }");
    }

    @Test
    public void refEq2() throws Exception {
        assertEvalTrue("interface I {} class C implements I {} { I i1 = new local C(); I i2 = new local C(); Bool testresult = !(i1 == i2); }");
    }

    @Test
    public void refGt() throws Exception {
        assertEvalTrue("interface I {} class C implements I {} { I i1 = new local C(); I i2 = new local C(); Bool testresult = i1 > i2 || i2 > i1; }");
    }

    @Test
    public void refLt() throws Exception {
        assertEvalTrue("interface I {} class C implements I {} { I i1 = new local C(); I i2 = new local C(); Bool testresult = i1 < i2 || i2 < i1; }");
    }

    @Test
    public void refGtEq() throws Exception {
        assertEvalTrue("interface I {} class C implements I {} { I i1 = new local C(); I i2 = new local C(); Bool testresult = i1 >= i2 || i1 < i2; }");
    }

    @Test
    public void refLtEq() throws Exception {
        assertEvalTrue("interface I {} class C implements I {} { I i1 = new local C(); I i2 = new local C(); Bool testresult = i1 <= i2 || i1 > i2; }");
    }

    @Test
    public void futLt() throws Exception {
        assertEvalTrue("interface I {Int m(Int i); Unit doit();}"
                + "class C implements I {Bool flag = False; Int m (Int i) {await flag; return i;} Unit doit() {flag = True;}}"
                + "{I i1 = new C(); I i2 = new C(); Bool reforder = i1 < i2; Int inti1 = when reforder then 1 else 2; Int inti2 = when reforder then 2 else 1; Fut<Int> fi1 = i1!m(inti1); Fut<Int> fi2 = i2!m(inti2); Bool futorder = fi1 < fi2; i1!doit(); i2!doit(); fi1.get; fi2.get; Bool testresult = when futorder then fi1 < fi2 else fi2 < fi1;}");
    }

    @Test
    public void refLtDiffClass() throws Exception {
        assertEvalTrue("interface I {}" + "class C1 implements I {}" + "class C2 implements I {}"
                + "{ I i1 = new C1(); I i2 = new C2(); Bool testresult = i1 < i2 || i2 < i1; }");
    }

    @Test
    public void classDecl() throws Exception {
        assertEvalTrue("class C { } { Bool testresult = True; }");
    }

    private static String EMPTY_CLASS_C = "interface I { } class C implements I { }";

    @Test
    public void intfClassDecl() throws Exception {
        assertEvalTrue(EMPTY_CLASS_C + "{ Bool testresult = True; I i; }");
    }

    @Test
    public void newExp() throws Exception {
        assertEvalTrue(EMPTY_CLASS_C + " { Bool testresult = True; I i = new local C();}");
    }

    private static String CLASS_WITH_METHOD = "interface I { Bool m(); } class C implements I { Bool m() { return True; } }";

    @Test
    public void methodCall() throws Exception {
        assertEvalTrue(CLASS_WITH_METHOD + " { Bool testresult = True; I i = new local C(); testresult = i.m();}");
    }

    private static String INTERFACE_I = "interface I { Bool m(); }";
    private static String CLASS_WITH_FIELD = INTERFACE_I
            + " class C(Bool f) implements I { Bool m() { return this.f; } }";

    private static String CALL_M = " { Bool testresult = True; I i = new local C(True); testresult = i.m();}";

    @Test
    public void fieldAccess() throws Exception {
        assertEvalTrue(CLASS_WITH_FIELD + CALL_M);
    }

    private static String CLASS_WITH_ASSIGN = INTERFACE_I
            + "class C(Bool f) implements I { Bool x = False; Bool m() { this.x = this.f; return this.x; } }";

    @Test
    public void fieldAssign() throws Exception {
        assertEvalTrue(CLASS_WITH_ASSIGN + CALL_M);
    }

    private static String CLASS_WITH_INITBLOCK = INTERFACE_I
            + "class C(Bool f) implements I { Bool x = False; { x = True; } Bool m() { return this.x; } }";

    @Test
    public void initBlock() throws Exception {
        assertEvalTrue(CLASS_WITH_INITBLOCK + CALL_M);
    }

    @Test
    public void classParamReadInFieldAssign() throws Exception {
        assertEvalTrue(INTERFACE_I + " class C(Bool f) implements I { Bool x = f; Bool m() { return this.x; } }"
                + CALL_M);
    }

    @Test
    public void classMethodShadowsField1() throws Exception {
        assertEvalTrue("interface I { Bool m(Bool f); } class C(Bool f) implements I { Bool m(Bool f) { return f; } }"
                + "{ Bool testresult = False; I i = new local C(False); testresult = i.m(True);}");
    }

    @Test
    public void classMethodShadowsField2() throws Exception {
        assertEvalTrue("interface I { Bool m(Bool f); } class C(Bool f) implements I { Bool m(Bool f) { return this.f; } }"
                + "{ Bool testresult = False; I i = new local C(True); testresult = i.m(False);}");
    }

    @Test
    public void traitApplication() throws Exception {
        // tests trait application
        assertEvalTrue(new File("abssamples/backend/ObjectTests/trait_application.abs"));
    }

    @Test
    public void traitOriginalCall() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ObjectTests/trait_original_call.abs"));
    }

    @Test
    public void fieldPatternMatch() throws Exception {
        assertEvalTrue(INTERFACE_I
                + "class C(Bool f) implements I { Int a=2; Int b=4; Bool m() { return case 2 {a => True;};  } }"
                + CALL_M);
    }

    @Test
    public void fieldPatternMatch2() throws Exception {
        assertEvalTrue(INTERFACE_I
                + "class C(Bool f) implements I { Int a=2; Int b=4; Bool m() { return case 2 {a => True;} && case 4{b=> True;};   } }"
                + CALL_M);
    }

    @Test
    public void fieldPatternMatchNestedSwitchStmt() throws Exception {
        assertEvalTrue(INTERFACE_I
                       + """
                         class C(Bool f) implements I {
                             Int a=2;
                             Int b=4;
                             Bool m() {
                                 Bool result = False;
                                 switch (2) {
                                     a => { Int u=7; switch (2) { a => result = True;}}
                                     b => { Int u=3; result=False;}}
                                 return result;
                             }
                         }
                         """
                + CALL_M);
    }

    @Test
    public void fieldPatternMatchNestedCaseExpr() throws Exception {
        assertEvalTrue(INTERFACE_I
                       + """
                         class C(Bool f) implements I {
                             Int a=2;
                             Int b=4;
                             Bool m() {
                                 return case 2 { a => case 2 { a => True }
                                               | b => False };
                             }
                         }
                         """
                       + CALL_M);
    }

    @Test
    public void testFutST1() throws Exception {
        assertEvalTrue("interface A {} interface B extends A {} interface I { A mA(); B mB(); } { switch (False) { True => { I o = null; Fut<A> f = o!mB();} False => {skip;}} Bool testresult = True; }");
    }
    
    @Test
    public void testIdentifiersAsKeywords() throws Exception {
        // https://github.com/abstools/abstools/issues/194
        // Erlang objected to "fun" as field / class parameter name.
        // throw in a "static" as well to try and trip up Java.
        assertEvalTrue("class A(Bool static) { Bool fun = False; } { Bool testresult = True; }");
    }

    @Test
    public void scheduler_priority() throws Exception {
        Assume.assumeTrue("Only meaningful with custom scheduler support", driver.supportsCustomSchedulers());
        assertEvalTrue(new File("abssamples/backend/TimeTests/scheduler_priority.abs"));
    }

    @Test
    public void classRecover1() throws Exception {
        Assume.assumeTrue("Only meaningful with exception support", driver.supportsExceptions());
        assertEvalTrue(new File("abssamples/backend/ObjectTests/recover1.abs"));
     }

    @Test
    public void syncCallAwaitField() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ObjectTests/synccall1.abs"));
    }

    @Test
    public void downcast_false() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ObjectTests/downcast-false.abs"));
    }

    @Test
    public void downcast_true() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ObjectTests/downcast-true.abs"));
    }

    @Test
    public void new_local_run_method() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ObjectTests/new_local_run_method.abs"));
    }

    @Test
    public void local_object_init_block_synccall() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ObjectTests/local_object_init_block_synccall.abs"));
    }
    @Test
    public void local_object_init_block_callback() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ObjectTests/local_object_init_block_callback.abs"));
    }
    @Test
    public void local_object_synccall_field_param() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ObjectTests/local_object_synccall_field_param.abs"));
    }
    // https://github.com/abstools/abstools/issues/251
    @Test
    public void implicit_unit_return() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ObjectTests/implicit_unit_return.abs"));
    }
    @Test
    public void common_superinterface() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ObjectTests/common_superinterface.abs"));
    }
}
