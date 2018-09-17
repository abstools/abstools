/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;

import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.typechecker.KindedName;
import org.abs_models.frontend.typechecker.ResolvedName;
import org.junit.Test;

import static org.junit.Assert.*;

public class AddImportsTest extends DeltaTest {

    @Test
    public void addImport() throws DeltaModellingException {
        Model model = assertParseOk(
                "module Exporter; export *;"
                + "interface I {}"
                + "interface J {}"
                + "module Exporter2; export *;"
                + "interface K {}"

                + "module M;"
                + "class C {}"

                + "delta D; uses M;"
                + "adds import Exporter.I;"
                + "adds import J from Exporter;"
                + "adds import * from Exporter2;"
                + "modifies class C { adds Exporter.I field1; } "
                + "modifies class C { adds          J field2; } "
                + "modifies class C { adds          K field3; } "
        );
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        DeltaDecl delta = findDelta(model, "D");
        model.applyDeltas(new ArrayList<>(Arrays.asList(delta)));

        ModuleDecl clsmodule = cls.getModuleDecl();
        Map<KindedName, ResolvedName> clsVisibleSymbols = clsmodule.getVisibleNames();
        KindedName symbol1 = new KindedName(KindedName.Kind.TYPE_DECL, "Exporter.I");
        KindedName symbol2 = new KindedName(KindedName.Kind.TYPE_DECL, "J");
        KindedName symbol3 = new KindedName(KindedName.Kind.TYPE_DECL, "K");
        assertTrue(clsVisibleSymbols.containsKey(symbol1));
        assertTrue(clsVisibleSymbols.containsKey(symbol2));
        assertTrue(clsVisibleSymbols.containsKey(symbol3));
    }

    @Test
    public void addExport() throws DeltaModellingException {
        Model model = assertParseOk(
                "module Exporter;"
                + "interface I {}"

                + "module M;"
                + "class C {}"

                + "delta D1; uses Exporter;"
                + "adds export I;"

                + "delta D2; uses M;"
                + "adds import Exporter.I;"
                + "modifies class C { adds Exporter.I field1; } "
        );
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        DeltaDecl d1 = findDelta(model, "D1");
        DeltaDecl d2 = findDelta(model, "D2");
        model.applyDeltas(new ArrayList<>(Arrays.asList(d1, d2)));

        ModuleDecl clsmodule = cls.getModuleDecl();
        Map<KindedName, ResolvedName> clsVisibleSymbols = clsmodule.getVisibleNames();
        KindedName symbol1 = new KindedName(KindedName.Kind.TYPE_DECL, "Exporter.I");
        assertTrue(clsVisibleSymbols.containsKey(symbol1));
    }

}
