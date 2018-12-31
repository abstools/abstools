/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ProductLine;
import org.junit.Test;

import static org.junit.Assert.*;

public class ProgramAbstractionTest extends DeltaTest {

    @Test
    public void core() {
        Model model = assertParse("module M;"
            + "class C(Rat myParam) { Int myField = 99; Int myMethod(String a, Bool b) { return 88; } }"
            + "productline PL;"
            + "features F;");
        SemanticConditionList errors = new SemanticConditionList();
        ProductLine pl = model.getProductLine();
        DeltaTrie trie = ProductLineAnalysisHelper.buildPFGT(pl, errors);
        ProgramAbstraction pa = trie.getRoot().getProgramAbstraction();

        assertTrue(pa.getClasses().containsKey("M.C"));

        assertEquals(2, pa.getClasses().get("M.C").get("fields").keySet().size());
        assertTrue(pa.getClasses().get("M.C").get("fields").containsKey("myParam"));
        assertEquals("Rat", pa.getClasses().get("M.C").get("fields").get("myParam").get(0));
        assertTrue(pa.getClasses().get("M.C").get("fields").containsKey("myField"));
        assertEquals("Int", pa.getClasses().get("M.C").get("fields").get("myField").get(0));

        assertTrue(pa.getClasses().get("M.C").get("methods").containsKey("myMethod"));
        assertEquals(3, pa.getClasses().get("M.C").get("methods").get("myMethod").size());
        assertEquals("Int", pa.getClasses().get("M.C").get("methods").get("myMethod").get(0));
        assertEquals("String", pa.getClasses().get("M.C").get("methods").get("myMethod").get(1));
        assertEquals("Bool", pa.getClasses().get("M.C").get("methods").get("myMethod").get(2));

    }

    @Test
    public void addClass() {
        Model model = assertParse("module M;"
            + "delta D;"
            + "uses M;"
            + "adds class C(Rat myParam) { Int myField = 99; Int myMethod(String a, Bool b) { return 88; } }"
            + "productline PL;"
            + "features F;"
            + "delta D when F;"
            + "root FM { group allof { opt F } }");
        SemanticConditionList errors = new SemanticConditionList();
        ProductLine pl = model.getProductLine();
        DeltaTrie trie = ProductLineAnalysisHelper.buildPFGT(pl, errors);
        ProgramAbstraction pa = trie.getRoot().getChildren().get("D").getProgramAbstraction();

        assertTrue(pa.getClasses().containsKey("M.C"));

        assertEquals(2, pa.getClasses().get("M.C").get("fields").keySet().size());
        assertTrue(pa.getClasses().get("M.C").get("fields").containsKey("myParam"));
        assertEquals("Rat", pa.getClasses().get("M.C").get("fields").get("myParam").get(0));
        assertTrue(pa.getClasses().get("M.C").get("fields").containsKey("myField"));
        assertEquals("Int", pa.getClasses().get("M.C").get("fields").get("myField").get(0));

        assertTrue(pa.getClasses().get("M.C").get("methods").containsKey("myMethod"));
        assertEquals(3, pa.getClasses().get("M.C").get("methods").get("myMethod").size());
        assertEquals("Int", pa.getClasses().get("M.C").get("methods").get("myMethod").get(0));
        assertEquals("String", pa.getClasses().get("M.C").get("methods").get("myMethod").get(1));
        assertEquals("Bool", pa.getClasses().get("M.C").get("methods").get("myMethod").get(2));
    }

    @Test
    public void removeClass() {
        Model model = assertParse("module M;"
            + "class C(Rat myParam) { Int myField = 99; Int myMethod(String a, Bool b) { return 88; } }"
            + "delta D;"
            + "uses M;"
            + "removes class C;"
            + "productline PL;"
            + "features F;"
            + "delta D when F;"
            + "root FM { group allof { opt F } }");
        SemanticConditionList errors = new SemanticConditionList();
        ProductLine pl = model.getProductLine();
        DeltaTrie trie = ProductLineAnalysisHelper.buildPFGT(pl, errors);
        ProgramAbstraction pa = trie.getRoot().getChildren().get("D").getProgramAbstraction();
        System.out.println("******\n" + pa);
        assertFalse(pa.getClasses().containsKey("M.C"));
    }

    @Test
    public void modifyClass() {
        Model model = assertParse("module M;"
            + "class C {}"
            + "delta D1; uses M;"
            + "modifies class C { adds Int myField = 99; }"
            + "delta D2; uses M;"
            + "modifies class C { removes Int myField; }"
            + "delta D3; uses M;"
            + "modifies class C { adds Int myMethod(String a, Bool b) { return 88; } }"
            + "delta D4; uses M;"
            + "modifies class C { modifies Int myMethod(String a, Bool b) { return 99; } }"
            + "delta D5; uses M;"
            + "modifies class C { removes Int myMethod(String a, Bool b); }"
            + "delta D6; uses M;"
            + "modifies class C adds I { }"
            + "delta D7; uses M;"
            + "modifies class C removes I { }"

            + "productline PL;"
            + "features F;"
            + "delta D1 when F;"
            + "delta D2 after D1 when F;"
            + "delta D3 after D2 when F;"
            + "delta D4 after D3 when F;"
            + "delta D5 after D4 when F;"
            + "delta D6 after D5 when F;"
            + "delta D7 after D6 when F;"
            + "root FM { group allof { opt F } }");
        SemanticConditionList errors = new SemanticConditionList();
        ProductLine pl = model.getProductLine();
        DeltaTrie trie = ProductLineAnalysisHelper.buildPFGT(pl, errors);

        // D1
        DeltaTrie.Node trieNode = trie.getRoot().getChildren().get("D1");
        ProgramAbstraction pa = trieNode.getProgramAbstraction();
        assertEquals(1, pa.getClasses().get("M.C").get("fields").keySet().size());
        assertTrue(pa.getClasses().get("M.C").get("fields").containsKey("myField"));
        assertEquals("Int", pa.getClasses().get("M.C").get("fields").get("myField").get(0));
        // D2
        trieNode = trieNode.getChildren().get("D2");
        pa = trieNode.getProgramAbstraction();
        assertEquals(0, pa.getClasses().get("M.C").get("fields").keySet().size());
        // D3
        trieNode = trieNode.getChildren().get("D3");
        pa = trieNode.getProgramAbstraction();
        assertTrue(pa.getClasses().get("M.C").get("methods").containsKey("myMethod"));
        assertEquals(3, pa.getClasses().get("M.C").get("methods").get("myMethod").size());
        assertEquals("Int", pa.getClasses().get("M.C").get("methods").get("myMethod").get(0));
        assertEquals("String", pa.getClasses().get("M.C").get("methods").get("myMethod").get(1));
        assertEquals("Bool", pa.getClasses().get("M.C").get("methods").get("myMethod").get(2));
        // D4
        trieNode = trieNode.getChildren().get("D4");
        pa = trieNode.getProgramAbstraction();
        assertTrue(pa.getClasses().get("M.C").get("methods").containsKey("myMethod"));
        assertEquals(3, pa.getClasses().get("M.C").get("methods").get("myMethod").size());
        assertEquals("Int", pa.getClasses().get("M.C").get("methods").get("myMethod").get(0));
        assertEquals("String", pa.getClasses().get("M.C").get("methods").get("myMethod").get(1));
        assertEquals("Bool", pa.getClasses().get("M.C").get("methods").get("myMethod").get(2));
        // D5
        trieNode = trieNode.getChildren().get("D5");
        pa = trieNode.getProgramAbstraction();
        assertEquals(0, pa.getClasses().get("M.C").get("methods").keySet().size());
        // D6
        trieNode = trieNode.getChildren().get("D6");
        pa = trieNode.getProgramAbstraction();
        assertTrue(pa.getClasses().get("M.C").get("interfaces").containsKey("I"));
        // D7
        trieNode = trieNode.getChildren().get("D7");
        pa = trieNode.getProgramAbstraction();
        assertEquals(0, pa.getClasses().get("M.C").get("interfaces").keySet().size());

        // TODO: add/remove interface
        // TODO modify interface: add/remove method; extends?

    }



}
