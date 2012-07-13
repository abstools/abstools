/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.Assert;
import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
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
        m.flattenForProduct("PeerToPeer.P1");
        m.flushCache();
        assertTrue(m.typeCheck().isEmpty());
    }

    @Test
    public void test_P2P_P2() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/PeerToPeer.abs", true);
        m.setNullPrintStream();
        m.flattenForProduct("PeerToPeer.P2");
        m.flushCache();
        assertTrue(m.typeCheck().isEmpty());
    }

    @Test
    public void test_P2P_P3() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/PeerToPeer.abs", true);
        m.setNullPrintStream();
        m.flattenForProduct("PeerToPeer.P3");
    }

    @Test
    public void test_P2P_P4() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/PeerToPeer.abs", true);
        m.setNullPrintStream();
        m.flattenForProduct("PeerToPeer.P4");
        m.flushCache();
        SemanticErrorList res = m.typeCheck();
        if (!res.isEmpty())
            fail(res.toString());
    }

    @Test
    public void test_ticket280() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/bug280.abs", true);
        m.setNullPrintStream();
        m.flattenForProduct("D.P");
        /* Run Maude gen (from the ticket), although it's not relevant. */
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        m.generateMaude(new PrintStream(out), null, 100, 0);
    }
    
    @Test
    public void test_ticket329() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug329.abs", true);
        SemanticErrorList errs = m.typeCheck();
        /* We are expecting a missing delta in product M.PL: */
        Assert.assertTrue(errs.getFirst() instanceof TypeError);
        TypeError te = (TypeError) errs.getFirst();
        Assert.assertEquals(ErrorMessage.NAME_NOT_RESOLVABLE, te.msg);
    }

    @Test
    public void test_ticket329_missingLineNo() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug329.abs", true);
        SemanticErrorList errs = m.typeCheck();
        /* We are expecting a missing delta in product M.PL: */
        Assert.assertTrue(errs.getFirst() instanceof TypeError);
        TypeError te = (TypeError) errs.getFirst();
        Assert.assertEquals(ErrorMessage.NAME_NOT_RESOLVABLE, te.msg);
        Assert.assertEquals(10, te.getLine());
    }

    @Test
    public void test_ticket324_A() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug324.abs", true);
        m.flattenForProduct("C.A");
        m.flushCache();
        Assert.assertTrue(!m.hasTypeErrors());
    }

    @Test
    public void test_ticket324_B() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug324.abs", true);
        m.flattenForProduct("C.B");
        m.flushCache();
        assertFalse(m.hasErrors());
        assertFalse(m.hasTypeErrors());
    }
    
    @Test
    public void test_ticket322_base() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug322.abs", true);
        assertFalse(m.hasErrors());
        assertFalse(m.hasTypeErrors());
    }
    @Test
    public void test_ticket322_P() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug322.abs", true);
        m.flattenForProduct("M.P");
        m.flushCache();
        assertFalse(m.hasErrors());
        assertFalse(m.hasTypeErrors());
        InterfaceDecl i = (InterfaceDecl) DeltaFlattenerTest.findDecl(m, "M", "I");
        assertNotNull(i);
        InterfaceDecl j = (InterfaceDecl) DeltaFlattenerTest.findDecl(m, "M", "J");
        assertNotNull(j);
    }
    @Test
    public void test_ticket322_Q() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/bug322.abs", true);
        m.flattenForProduct("M.Q");
        m.flushCache();
        assertFalse(m.hasErrors());
        assertFalse(m.hasTypeErrors());
        InterfaceDecl i = (InterfaceDecl) DeltaFlattenerTest.findDecl(m, "M", "I");
        assertNull(i);
        InterfaceDecl j = (InterfaceDecl) DeltaFlattenerTest.findDecl(m, "M", "J");
        assertNull(j);
    }
    
    @Test
    public void test_ticket361_base() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/ticket361.abs", true);
        assertFalse(m.hasErrors());
        assertFalse(m.hasTypeErrors());
    }
    @Test
    public void test_ticket361_P() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/ticket361.abs", true);
        m.flattenForProduct("M3.P");
        m.flushCache();
        assertFalse(m.hasErrors());
        assertFalse(m.hasTypeErrors());
    }

    @Test
    public void test_ticket332_base() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/ticket332.abs", true);
        assertFalse(m.hasParserErrors());
        assertFalse(m.hasErrors());
        assertTrue(m.typeCheck().isEmpty());
    }

    @Test
    public void test_ticket332_parse() throws Exception {
        Model m = assertParseFileOk("tests/abssamples/deltas/ticket332.abs", true);
        m.flushCache();
        m.flattenForProduct("DE.C");
        m.flushCache();
        assertTrue(m.typeCheck().isEmpty());
    }

    @Test
    public void test_ticket332_check() throws Exception {
        Model m = assertTypeCheckFileOk("tests/abssamples/deltas/ticket332.abs", true);
        m.flushCache();
        m.flattenForProduct("DE.C");
        m.flushCache();
        assertTrue(m.typeCheck().isEmpty());
    }
}
