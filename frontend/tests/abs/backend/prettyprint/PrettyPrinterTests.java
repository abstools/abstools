/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.prettyprint;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import org.junit.Before;
import org.junit.Test;

import abs.frontend.ast.DeltaDecl;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;
import abs.frontend.tests.ABSFormatter;
import abs.frontend.tests.EmptyFormatter;
import beaver.Parser.Exception;

public class PrettyPrinterTests {
    
    private ABSParser parser;

    @Before
    public void setUp() {
        parser = new ABSParser();
    }

    @Test
    public void prettyPrinterAddDataTypeModifierTest() throws IOException, Exception{
        String deltaDecl = "delta Foo{adds data States=F|B|I|M;}";
        ABSScanner scanner = new ABSScanner(new StringReader(deltaDecl));
        DeltaDecl d = (DeltaDecl) parser.parse(scanner,ABSParser.AltGoals.delta_decl);
        assertEquals("deltaFoo(){addsdataStates=F|B|I|M;}", replaceWhitespaceChars(prettyPrint(d)));
    }
    
    @Test
    public void prettyPrinterModifyInterfaceModifierTest() throws IOException, Exception{
        String deltaDecl = "delta Foo{modifies interface X{removes Int fooMethod();adds Int fooMethod();}}";
        ABSScanner scanner = new ABSScanner(new StringReader(deltaDecl));
        DeltaDecl d = (DeltaDecl) parser.parse(scanner,ABSParser.AltGoals.delta_decl);
        assertEquals("deltaFoo(){modifiesinterfaceX{removesIntfooMethod();addsIntfooMethod();}}", replaceWhitespaceChars(prettyPrint(d)));
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
