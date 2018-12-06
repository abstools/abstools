/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.prettyprint;

import static org.junit.Assert.assertEquals;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.parser.Main;
import org.abs_models.frontend.tests.ABSFormatter;
import org.abs_models.frontend.tests.EmptyFormatter;
import org.junit.Test;

import org.abs_models.ABSTest;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Model;

public class PrettyPrinterTests extends ABSTest {

    @Test
    public void prettyPrinterAddDataTypeModifierTest() throws Exception{
        String deltaDecl = "delta Foo;adds data States=F|B|I|M;";
        DeltaDecl d = (DeltaDecl) Main
            .parseUnit(null, new StringReader(deltaDecl), true, false).getDeltaDecl(0);
        assertEquals("deltaFoo;addsdataStates=F|B|I|M;", replaceWhitespaceChars(prettyPrint(d)));
    }

    @Test
    public void prettyPrinterModifyInterfaceModifierTest() throws Exception{
        String deltaDecl = "delta Foo;modifies interface X{removes Int fooMethod();adds Int fooMethod();}";
        Model m = assertParseOk(deltaDecl, Config.WITHOUT_MODULE_NAME);
        assertEquals("deltaFoo;modifiesinterfaceX{removesIntfooMethod();addsIntfooMethod();}", replaceWhitespaceChars(prettyPrint(m)));
    }
    
    @Test
    public void prettyPrinterListLiteralTest() throws Exception {
        String ms = "module Test; { List<Int> x = list[1, 2, 3]; }";
        Model m = assertParseOk(ms, Config.WITHOUT_MODULE_NAME);
        assertEquals("moduleTest;{List<Int>x=list[1,2,3];}", replaceWhitespaceChars(prettyPrint(m)));
    }

    @Test
    public void prettyPrinterFloatLiteralTest() throws Exception {
        String ms = "module Test; { Float x = 3.1415927; }";
        Model m = assertParseOk(ms, Config.WITHOUT_MODULE_NAME);
        assertEquals("moduleTest;{Floatx=3.1415927;}", replaceWhitespaceChars(prettyPrint(m)));
    }

    private String prettyPrint(ASTNode<?> d) {
        StringWriter writer = new StringWriter();
        PrintWriter w = new PrintWriter(writer);
        ABSFormatter f = new EmptyFormatter();
        d.doPrettyPrint(w,f);
        return writer.toString();
    }

    private String replaceWhitespaceChars(String in){
        return in.replace("\n", "").replace("\r", "").replace(" ", "");
    }

}
