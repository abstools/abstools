/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.prettyprint;

import static org.junit.Assert.assertEquals;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import org.junit.Test;

import abs.frontend.antlr.parser.ABSParserWrapper;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.tests.ABSFormatter;
import abs.frontend.tests.EmptyFormatter;

public class PrettyPrinterTests {

    @Test
    public void prettyPrinterAddDataTypeModifierTest() throws Exception{
        String deltaDecl = "delta Foo;adds data States=F|B|I|M;";
        DeltaDecl d = (DeltaDecl) new ABSParserWrapper(null, true, false)
            .parse(new StringReader(deltaDecl)).getDeltaDecl(0); 
        assertEquals("deltaFoo;addsdataStates=F|B|I|M;", replaceWhitespaceChars(prettyPrint(d)));
    }

    @Test
    public void prettyPrinterModifyInterfaceModifierTest() throws Exception{
        String deltaDecl = "delta Foo;modifies interface X{removes Int fooMethod();adds Int fooMethod();}";
        DeltaDecl d = (DeltaDecl) new ABSParserWrapper(null, true, false)
            .parse(new StringReader(deltaDecl)).getDeltaDecl(0);
        assertEquals("deltaFoo;modifiesinterfaceX{removesIntfooMethod();addsIntfooMethod();}", replaceWhitespaceChars(prettyPrint(d)));
    }

    private String prettyPrint(DeltaDecl d) {
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
