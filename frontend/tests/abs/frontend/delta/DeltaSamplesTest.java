/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import abs.backend.maude.MaudeCompiler;
import abs.common.WrongProgramArgumentException;
import abs.frontend.FrontendTest;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.Model;

public class DeltaSamplesTest extends FrontendTest {

    @Test
    public void test_P2P_tc() throws Exception {
        /* Base case, check for valid input */
        assertTypeCheckFileOk("tests/abssamples/deltas/PeerToPeer.abs", true);
    }

    @Test
    public void test_P2P_P1() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/PeerToPeer.abs", true);
        m.setNullPrintStream();
        m.flattenForProduct("P1");
        m.flushCache();
        assertTrue(!m.typeCheck().containsErrors());
    }

    @Test
    public void test_P2P_P2() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/PeerToPeer.abs", true);
        m.setNullPrintStream();
        m.flattenForProduct("P2");
        m.flushCache();
        assertTrue(!m.typeCheck().containsErrors());
    }

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    /*   @Test
    public void test_P2P_P3() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/PeerToPeer.abs", true);
        m.setNullPrintStream();
        thrown.expect(DeltaModellingWithNodeException.class);
        m.flattenForProduct("P3");
    }
*/
    @Test
    public void test_P2P_P4() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/PeerToPeer.abs", true);
        m.setNullPrintStream();
        m.flattenForProduct("P4");
        m.flushCache();
        SemanticConditionList res = m.typeCheck();
        if (res.containsErrors())
            fail(res.getFirstError().getMessage());
    }

    @Test
    public void test_ticket280() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/bug280.abs", true);
        m.setNullPrintStream();
        m.flattenForProduct("P");
        /* Run Maude gen (from the ticket), although it's not relevant. */
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        m.generateMaude(new PrintStream(out), MaudeCompiler.SIMULATOR.RL, 100, 0);
    }
    
    @Test
    public void test_ticket329() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug329.abs", true);
        SemanticConditionList errs = m.typeCheck();
        /* We are expecting a missing delta in product M.PL: */
        assertThat(errs.getFirstError(), instanceOf(TypeError.class));
        TypeError te = (TypeError) errs.getFirstError();
        Assert.assertEquals(ErrorMessage.NAME_NOT_RESOLVABLE, te.msg);
    }

    @Test
    public void test_ticket329_missingLineNo() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug329.abs", true);
        SemanticConditionList errs = m.typeCheck();
        /* We are expecting a missing delta in product M.PL: */
        assertThat(errs.getFirstError(), instanceOf(TypeError.class));
        TypeError te = (TypeError) errs.getFirstError();
        Assert.assertEquals(ErrorMessage.NAME_NOT_RESOLVABLE, te.msg);
        Assert.assertEquals(10, te.getLine());
    }

    @Test
    public void test_ticket324_A() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug324.abs", true);
        m.flattenForProduct("A");
        m.flushCache();
        assertFalse(m.hasTypeErrors());
    }

    @Test(expected=DeltaModellingException.class)
    public void test_ticket324_B() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug324.abs", true);
        m.flattenForProduct("B");
    }

    @Test
    public void test_ticket322_P() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/bug322.abs", true);
        m.flattenForProduct("P");
        m.flushCache();
        assertFalse(m.hasErrors());
        assertFalse(m.hasTypeErrors());
        InterfaceDecl i = (InterfaceDecl) DeltaTest.findDecl(m, "M", "I");
        assertNotNull(i);
        InterfaceDecl j = (InterfaceDecl) DeltaTest.findDecl(m, "M", "J");
        assertNotNull(j);
    }
    @Test
    public void test_ticket322_Q() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/bug322.abs", true);
        m.flattenForProduct("Q");
        m.flushCache();
        assertFalse(m.hasErrors());
        assertFalse(m.hasTypeErrors());
        InterfaceDecl i = (InterfaceDecl) DeltaTest.findDecl(m, "M", "I");
        assertNull(i);
        InterfaceDecl j = (InterfaceDecl) DeltaTest.findDecl(m, "M", "J");
        assertNull(j);
    }

    @Test
    public void test_ticket361_P() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/ticket361.abs", true);
        m.flattenForProduct("P");
        m.flushCache();
        if (m.hasErrors())
            fail(m.getErrors().getFirstError().getMessage());
        assertFalse(m.hasTypeErrors());
    }

    @Test
    public void test_ticket332_parse() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/ticket332.abs", true);
        m.flushCache();
        m.flattenForProduct("C");
        m.flushCache();
        assertTrue(!m.typeCheck().containsErrors());
    }

    @Test
    public void test_ticket332_check() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/ticket332.abs", true);
        m.flushCache();
        m.flattenForProduct("C");
        m.flushCache();
        assertTrue(!m.typeCheck().containsErrors());
    }
}
