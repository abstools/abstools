/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.prettyprint;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.abs_models.ABSTest;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;
import org.junit.Test;

public class PrettyPrinterTests extends ABSTest {

    @Test
    public void prettyPrinterAddDataTypeModifierTest() throws Exception{
        String deltaDecl = "delta Foo;adds data States=F|B|I|M;";
        DeltaDecl d = (DeltaDecl) Main
            .parseUnit(null, new StringReader(deltaDecl), true).getDeltaDecl(0);
        assertEqualsAndParses(deltaDecl, d);
    }

    @Test
    public void prettyPrinterModifyInterfaceModifierTest() throws Exception{
        String deltaDecl = "delta Foo;modifies interface X{removes Int fooMethod();adds Int fooMethod();}";
        Model m = assertParse(deltaDecl, Config.WITHOUT_MODULE_NAME, Config.WITHOUT_DESUGARING);
        assertEqualsAndParses(deltaDecl, m);
    }
    
    @Test
    public void prettyPrinterListLiteralTest() throws Exception {
        String ms = "module Test; { List<Int> x = list[1, 2, 3]; }";
        Model m = assertParse(ms, Config.WITHOUT_MODULE_NAME, Config.WITHOUT_DESUGARING);
        assertEqualsAndParses(ms, m);
    }

    @Test
    public void prettyPrinterFloatLiteralTest() throws Exception {
        String ms = "module Test; { Float x = 3.1415927; }";
        Model m = assertParse(ms, Config.WITHOUT_MODULE_NAME, Config.WITHOUT_DESUGARING);
        assertEqualsAndParses(ms, m);
    }

    @Test
    public void prettyPrinterLiterals() throws Exception {
        String ms = readFile("abssamples/backend/PrettyPrinterTests/Literals.abs");
        Model m = assertParse(ms, Config.WITHOUT_MODULE_NAME, Config.WITHOUT_DESUGARING);
        assertEqualsAndParses(ms, m);
    }

    @Test
    public void prettyPrinterExpressions() throws Exception {
        // TODO: inhibit more rewriting
        // Untested until then:
        // - ParFnApp
        // - OriginalCall
        String ms = readFile("abssamples/backend/PrettyPrinterTests/PureExpressions.abs");
        Model m = assertParse(ms, Config.WITHOUT_MODULE_NAME, Config.WITHOUT_DESUGARING);
        assertEqualsAndParses(ms, m);
        ms = readFile("abssamples/backend/PrettyPrinterTests/EffExpressions.abs");
        m = assertParse(ms, Config.WITHOUT_MODULE_NAME, Config.WITHOUT_DESUGARING);
        assertEqualsAndParses(ms, m);
    }

    @Test
    public void prettyPrinterStatements() throws Exception {
        String ms = readFile("abssamples/backend/PrettyPrinterTests/Statements.abs");
        Model m = assertParse(ms, Config.WITHOUT_MODULE_NAME, Config.WITHOUT_DESUGARING);
        assertEqualsAndParses(ms, m);
    }

    private static void assertEqualsAndParses(String s, ASTNode<?> m) {
        assertEquals(replaceWhitespaceChars(s), replaceWhitespaceChars(prettyPrint(m)));
        assertParse(prettyPrint(m), Config.WITHOUT_MODULE_NAME, Config.WITHOUT_DESUGARING);
    }

    private static String readFile(String filename) throws IOException
    {
        byte[] encoded = Files.readAllBytes(Paths.get(resolveFileName(filename)));
        return new String(encoded, StandardCharsets.UTF_8);
    }

    private static String prettyPrint(ASTNode<?> d) {
        StringWriter writer = new StringWriter();
        PrintWriter w = new PrintWriter(writer);
        ABSFormatter f = new DefaultABSFormatter(w);
        d.doPrettyPrint(w,f);
        return writer.toString();
    }

    private static String replaceWhitespaceChars(String in){
        return in.replace("\n", "").replace("\r", "").replace(" ", "");
    }

}
