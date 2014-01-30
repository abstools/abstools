/** 
 * Copyright (c) 2013
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import abs.backend.common.SemanticTests;

/**
 * Note that this class doesn't have a nullary constructor and can't be run as a
 * JUnit test itself!
 */
public class ErlangErrorHandlingTests extends ErlangTestDriver {

    @BeforeClass
    public static void checkRequired() {
        Assert.assertTrue(SemanticTests.checkErlang());
    }

    private final static String CLASS_WITH_METHOD = "module BackendTest; interface I { Bool ok(); Bool a(); Bool d(); }"
            + " class C implements I { Bool ok() { return True; } Bool a(){abort \"err\";return False;} Bool d(){die \"die\";return False;}}";

    @Test
    public void simpleAbort() throws Exception {
        assertEvalFails("module BackendTest;{ Bool testresult = True;  abort \"aa\";}");
    }

    @Test
    public void classWithErrorPrimitves() throws Exception {
        assertEvalTrue(CLASS_WITH_METHOD + "{ Bool testresult = True; I c=new C(); testresult = await c!ok(); }");
    }

    @Test
    public void errorProp() throws Exception {
        assertEvalFails(CLASS_WITH_METHOD + "{ Bool testresult = True; I c=new C(); testresult = await c!a(); }");
    }

    @Test
    public void safeGet() throws Exception {
        assertEvalTrue(CLASS_WITH_METHOD + "{ Bool testresult = True; I c=new C(); Fut<Bool> f= c!a(); "
                + "Result<Bool> r= f.safeget; testresult =  r == Error(\"err\");   }");
    }

    @Test
    public void safeGetDie() throws Exception {
        assertEvalTrue(CLASS_WITH_METHOD + "{ Bool testresult = True; I c=new C(); Fut<Bool> f= c!d(); "
                + "Result<Bool> r= f.safeget; testresult =  r == Error(\"die\");   }");
    }

    @Test
    public void safeGetDieDeadObject() throws Exception {
        assertEvalTrue(CLASS_WITH_METHOD + "{ Bool testresult = True; I c=new C(); Fut<Bool> f= c!d(); "
                + "Result<Bool> r= f.safeget; testresult =  r == Error(\"die\"); "
                + "f=c!ok(); r=f.safeget; testresult= testresult && r== Error(\"deadObject\");  }");
    }

    @Test
    public void mainSyncCallDeadObject() throws Exception {
        assertEvalFails(CLASS_WITH_METHOD
                + "{I c= new local C(); Fut<Bool> f=  c!d(); await f?; Bool testresult= c.ok();}");
    }

    @Test
    public void asyncCallSyncCallDeadObject() throws Exception {
        assertEvalTrue(CLASS_WITH_METHOD
                + "interface Proxy { Unit c(I obj);} class P implements Proxy{Unit c (I obj) {obj.ok();}}"
                + "{I c= new local C(); Fut<Bool> f=  c!d(); await f?;"
                + " Proxy p= new local P(); Fut<Unit> f1=p!c(c); await f1?; Result<Unit> r=f1.safeget ;"
                + "Bool testresult= r== Error(\"deadObject\");}");
    }

    @Test
    public void mainCallSyncDie() throws Exception {
        assertEvalTrue(CLASS_WITH_METHOD
                + "interface Proxy { Unit c(I obj);} class P implements Proxy{Unit c (I obj) {obj.ok();}}"
                + "{I c= new local C(); c.d();"
                + "Proxy p= new local P(); Fut<Unit> f1=p!c(c); await f1?; Result<Unit> r=f1.safeget ;"
                + "Bool testresult= r== Error(\"deadObject\");}");
    }

    private final static String ACTIVE_CLASS = "module BackendTest; interface I { Bool ok(); Bool wait(); Bool d(); }"
            + " class C implements I { Bool should_die=False; Unit run() { await should_die; abort \"active\";} Bool ok() { return True; } Bool wait(){await False;return False;} Bool d(){should_die=True;return True;}}";

    @Test
    public void activeObjectDead() throws Exception {
        assertEvalTrue(ACTIVE_CLASS + "{ Bool testresult = True; I c=new C(); Fut<Bool> f= c!wait(); "
                + "c!d();Result<Bool> r= f.safeget; testresult =  r == Error(\"active\"); "
                + "f=c!ok(); r=f.safeget; testresult= testresult && r== Error(\"deadObject\");  }");
    }

    private final static String ROLLBACK_CLASS = "module BackendTest; interface I { Unit inc(); Unit incWaitIncFail(); Int g(); }"
            + " class C implements I { Int count=0; Unit inc(){count=count+1;} Unit incWaitIncFail(){ count=count+1;suspend; count=count+1; abort \"err\";} Int g(){return count;}}";

    @Test
    public void objectRollback() throws Exception {
        assertEvalTrue(ROLLBACK_CLASS + "{ Bool testresult = True; I c=new C(); await c!inc();  "
                + "Int r=await c!g(); testresult = r==1;"
                + "Fut<Unit> f=c!incWaitIncFail(); await f?; r=await c!g();  testresult= testresult && r==2;  }");
    }
}
