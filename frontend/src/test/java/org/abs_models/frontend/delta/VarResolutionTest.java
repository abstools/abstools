/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;

import java.util.HashSet;

import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.KindedName;
import org.abs_models.frontend.typechecker.UnknownType;
import org.junit.Test;

public class VarResolutionTest extends DeltaTest {

    @Test
    public void fromAddFieldModifierToCoreTest() {
        Model model = assertParse("module M;"
            + "class C {"
            + "Int x = 0;"
            + "}"
            + "delta D; uses M;"
            + "modifies class C {"
            + "adds Int y = x;"
            + "}");
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        AddFieldModifier mod = (AddFieldModifier) mmod.getModifier(0);
        // until a delta is applied, we may not be able to tell vars from fields:
        assertThat(mod.getFieldDecl().getInitExp() , instanceOf(VarUse.class));
        assertEquals(UnknownType.INSTANCE,mod.getFieldDecl().getInitExp().getType());
    }

    @Test
    public void fromAddFieldModifierToCoreTest2() {
        Model model = assertParse("module M;"
            + "class C {"
            + "Int x = 0;"
            + "}"
            + "delta D;uses M;"
            + "modifies class C {"
            + "adds Int y = x;"
            + "}");
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        mmod.apply();
        ModuleDecl m = model.lookupModule("M");
        ClassDecl c = (ClassDecl) m.lookup(new KindedName(KindedName.Kind.CLASS, "C"));
        FieldDecl fy = (FieldDecl) c.locallookupVarOrFieldName("y", true);
        assertThat(fy.getInitExp() , instanceOf(FieldUse.class));
    }
    @Test
    public void fromAddMethodModifierToCoreTest() {
        Model model = assertParse("module M;"
            + "class C {"
            + "Int x = 0;"
            + "}"
            + "delta D;uses M;"
            + "modifies class C {"
            + "adds Int getX() { return x; }"
            + "}");
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        DeltaTraitModifier mod = (DeltaTraitModifier) mmod.getModifier(0);
        AddMethodModifier opr =  (AddMethodModifier)mod.getMethodModifier();
        TraitSetExpr expr = (TraitSetExpr)opr.getTraitExpr();
        ReturnStmt stmt = (ReturnStmt) expr.getMethodImpl(0).getBlock().getStmt(0);
        assertThat(stmt.getRetExp(), instanceOf(FieldUse.class));
    }

    @Test
    public void fromModifyMethodModifierToCoreTest() {
        Model model = assertParse("module M;"
            + "class C {"
            + "Int x = 0;"
            + "Int getX() { }"
            + "}"
            + "delta D;uses M;"
            + "modifies class C {"
            + "modifies Int getX() { return x; }"
            + "}");
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        DeltaTraitModifier mod = (DeltaTraitModifier) mmod.getModifier(0);
        ModifyMethodModifier opr =  (ModifyMethodModifier)mod.getMethodModifier();
        TraitSetExpr expr = (TraitSetExpr)opr.getTraitExpr();
        ReturnStmt stmt = (ReturnStmt) expr.getMethodImpl(0).getBlock().getStmt(0);
        assertThat(stmt.getRetExp(), instanceOf(FieldUse.class));
    }
    
    @Test
    public void fromModifierToSameClassModifierTest() {
        Model model = assertParse("module M;"
            + "class C { }"
            + "delta D;uses M;"
            + "modifies class C {"
            + "adds Int x = 0;"
            + "adds Int getX() { return x; }"
            + "}");
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        DeltaTraitModifier mod = (DeltaTraitModifier) mmod.getModifier(1);
        AddMethodModifier opr =  (AddMethodModifier)mod.getMethodModifier();
        TraitSetExpr expr = (TraitSetExpr)opr.getTraitExpr();
        ReturnStmt stmt = (ReturnStmt) expr.getMethodImpl(0).getBlock().getStmt(0);
        assertThat(stmt.getRetExp(), instanceOf(FieldUse.class));
    }

    @Test
    public void fromModifierToOtherModifyClassModifierTest() {
        Model model = assertParse("module M;"
            + "class C { }"
            + "delta D;"
            + "uses M;"
            + "modifies class C {"
            + "adds Int x = 0;"
            + "}"
            + " modifies class C {"
            + "adds Int getX() { return x; }"
            + "}");
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(1);
        DeltaTraitModifier mod = (DeltaTraitModifier) mmod.getModifier(0);
        AddMethodModifier opr =  (AddMethodModifier)mod.getMethodModifier();
        TraitSetExpr expr = (TraitSetExpr)opr.getTraitExpr();
        ReturnStmt stmt = (ReturnStmt) expr.getMethodImpl(0).getBlock().getStmt(0);
        assertThat(stmt.getRetExp(), instanceOf(FieldUse.class));
    }

    @Test
    public void fromModifierToOtherAddClassModifierTest() {
        Model model = assertParse("module M;"
            + "delta D;"
            + "uses M;"
            + "adds class C {"
            + "Int x = 0;"
            + "}"

            + "modifies class C {"
            + "adds Int getX() { return x; }"
            + "}");
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(1);
        DeltaTraitModifier mod = (DeltaTraitModifier) mmod.getModifier(0);
        AddMethodModifier opr =  (AddMethodModifier)mod.getMethodModifier();
        TraitSetExpr expr = (TraitSetExpr)opr.getTraitExpr();
        ReturnStmt stmt = (ReturnStmt) expr.getMethodImpl(0).getBlock().getStmt(0);
        assertThat(stmt.getRetExp(), instanceOf(FieldUse.class));
    }

    @Test
    public void nonFieldTest() {
        Model model = assertParse("module M;"
            + "class C {"
            + "Int x = 0;"
            + "}"
            + "delta D;"
            + "uses M;"
            + "modifies class C {"
            + "adds Int getX() { Int x = 17; return x; }"
            + "}");
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        DeltaTraitModifier mod = (DeltaTraitModifier) mmod.getModifier(0);
        AddMethodModifier opr =  (AddMethodModifier)mod.getMethodModifier();
        TraitSetExpr expr = (TraitSetExpr)opr.getTraitExpr();
        ReturnStmt stmt = (ReturnStmt) expr.getMethodImpl(0).getBlock().getStmt(1);
        assertThat(stmt.getRetExp(), instanceOf(VarUse.class));
    }
    
    @Test
    public void defUseMultipleFiles() throws Exception {
    Model m = this.assertParseFilesOk(new HashSet<String>() {{ add("abssamples/deltas/defuse/def.abs"); add("abssamples/deltas/defuse/use.abs");}});
    m.collapseTraitModifiers();
    m.evaluateAllProductDeclarations();  
    m.flushCache();
    m.flattenForProduct("Prod");
    m.flushCache();
    assertTrue(!m.typeCheck().containsErrors());
    }
}
