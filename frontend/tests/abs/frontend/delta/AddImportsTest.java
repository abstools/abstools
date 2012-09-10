/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;

import org.junit.Test;
import static org.junit.Assert.*;

import abs.frontend.ast.*;
import abs.frontend.typechecker.*;
import abs.frontend.delta.exceptions.*;


public class AddImportsTest extends DeltaTest {
    
    @Test
    public void addQualImport() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M1; export *;"
                + "class C {}"
                
                + "module M2; export *;"
                + "interface I { Unit m(); }"
                
                + "module D;"
                + "import * from M1;"
                + "import * from M2;"
                
                + "delta D;"
                + "modifies class C implements M2.I { adds Unit m() {} } "
        );
        ClassDecl cls = (ClassDecl) findDecl(model, "M1", "C");
        DeltaDecl delta = findDelta(model, "D");
        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(delta)));
        
        // the compiler needs to add an "import M2.I" to M1
        ModuleDecl clsmodule = cls.getModule();
        Map<KindedName, ResolvedName> clsVisibleSymbols = clsmodule.getVisibleNames();
        KindedName symbol = new KindedName(KindedName.Kind.TYPE_DECL, "M2.I");
        assertTrue(clsVisibleSymbols.containsKey(symbol));
    }

    @Test
    public void addQualImport2() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M; export *;"
                + "interface I { Unit m(); }"
                
                + "module D;"
                + "import * from M;"
                + "delta D;"
                + "adds class C implements M.I { Unit m() {} }"
        );
        DeltaDecl delta = findDelta(model, "D");
        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(delta)));
        
        // the compiler doesn't need to add anything 
        ClassDecl cls = (ClassDecl) findDecl(model, "D", "C");
        ModuleDecl clsmodule = cls.getModule();
        Map<KindedName, ResolvedName> clsVisibleSymbols = clsmodule.getVisibleNames();
        KindedName symbol = new KindedName(KindedName.Kind.TYPE_DECL, "M.I");
        assertTrue(clsVisibleSymbols.containsKey(symbol));
        symbol = new KindedName(KindedName.Kind.TYPE_DECL, "I");
        assertTrue(clsVisibleSymbols.containsKey(symbol));
    }

    @Test
    public void addQualImport3() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M; export *;"
                + "interface I { Unit m(); }"
                + "class C {}" 
                
                + "module D;"
                + "import * from M;"
                + "delta D;"
                + "adds class C2 implements I { Unit m() {} }"
                + "modifies class C { adds Unit n() { I obj = new D.C2(); } }"
        );
        DeltaDecl delta = findDelta(model, "D");
        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(delta)));
        
        // the compiler needs to add an "import D.C2" to M
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        ModuleDecl clsmodule = cls.getModule();
        Map<KindedName, ResolvedName> clsVisibleSymbols = clsmodule.getVisibleNames();
        
        // TODO
//        KindedName symbol = new KindedName(KindedName.Kind.TYPE_DECL, "D.C2");
//        assertTrue(clsVisibleSymbols.containsKey(symbol));
//        symbol = new KindedName(KindedName.Kind.TYPE_DECL, "C2");
//        assertTrue(clsVisibleSymbols.containsKey(symbol));
    }
    
    @Test
    public void addUnqualImport() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M1; export *;"
                + "class C {}"
                
                + "module M2; export *;"
                + "interface I { Unit m(); }"
                
                + "module MD;"
                + "import * from M1;"
                + "import * from M2;"
                + "delta D;"
                + "modifies class C implements I { adds Unit m() {} } "
        );
        
        // the compiler needs to add an "import I from M2" to M1 
        // TODO
        
    }

    @Test
    public void doNotAddImport() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M1; export *;"
                + "class C {}"
                
                + "module M2; export *;"
                + "interface I { Unit m(); }"
                
                + "module D;"
                + "import * from M1;"
                + "delta D;"
                + "modifies class C implements M2.I { adds Unit m() {} } "
        );
        
        ClassDecl cls = (ClassDecl) findDecl(model, "M1", "C");
        DeltaDecl delta = findDelta(model, "D");
        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(delta)));

        // the compiler should not add an import, because the delta cannot see I!
        ModuleDecl clsmodule = cls.getModule();
        Map<KindedName, ResolvedName> clsVisibleSymbols = clsmodule.getVisibleNames();
        KindedName symbol = new KindedName(KindedName.Kind.TYPE_DECL, "M2.I");
        assertFalse(clsVisibleSymbols.containsKey(symbol));
    }

}