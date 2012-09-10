/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.ast.*;

public class VarResolutionTest extends DeltaTest {

    @Test
    public void fromAddFieldModifierToCoreTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C {"
                + "Int x = 0;"
                + "}"
                + "delta D;"
                + "modifies class M.C {"
                + "adds Int y = x;"
                + "}"
        );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        AddFieldModifier mod = (AddFieldModifier) mmod.getModifier(0);
        assertTrue(mod.getFieldDecl().getInitExp() instanceof FieldUse);
    }
    
    @Test
    public void fromAddMethodModifierToCoreTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C {"
                + "Int x = 0;"
                + "}"
                + "delta D;"
                + "modifies class M.C {"
                + "adds Int getX() { return x; }"
                + "}"
        );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        AddMethodModifier mod = (AddMethodModifier) mmod.getModifier(0);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(0);
        assertTrue(stmt.getRetExp() instanceof FieldUse);
    }

    @Test
    public void fromModifyMethodModifierToCoreTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C {"
                + "Int x = 0;"
                + "Int getX() { }"
                + "}"
                + "delta D;"
                + "modifies class M.C {"
                + "modifies Int getX() { return x; }"
                + "}"
                );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        ModifyMethodModifier mod = (ModifyMethodModifier) mmod.getModifier(0);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(0);
        assertTrue(stmt.getRetExp() instanceof FieldUse);
    }
    
    @Test
    public void fromModifierToSameClassModifierTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C { }"
                + "delta D;"
                + "modifies class M.C {"
                + "adds Int x = 0;"
                + "adds Int getX() { return x; }"
                + "}"
                );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        AddMethodModifier mod = (AddMethodModifier) mmod.getModifier(1);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(0);
        assertTrue(stmt.getRetExp() instanceof FieldUse);
    }
    
    @Test
    public void fromModifierToOtherModifyClassModifierTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C { }"
                + "delta D;"
                + "modifies class M.C {"
                + "adds Int x = 0;"
                + "}"
                + "modifies class M.C {"
                + "adds Int getX() { return x; }"
                + "}"
                );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(1);
        AddMethodModifier mod = (AddMethodModifier) mmod.getModifier(0);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(0);
        assertTrue(stmt.getRetExp() instanceof FieldUse);
    }

    @Test
    public void fromModifierToOtherAddClassModifierTest() {
        Model model = assertParseOk(
                "module M;"
                + "delta D;"
                + "adds class M.C {"
                + "Int x = 0;"
                + "}"
                + "modifies class M.C {"
                + "adds Int getX() { return x; }"
                + "}"
                );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(1);
        AddMethodModifier mod = (AddMethodModifier) mmod.getModifier(0);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(0);
        assertTrue(stmt.getRetExp() instanceof FieldUse);
    }

    @Test
    public void nonFieldTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C {"
                + "Int x = 0;"
                + "}"
                + "delta D;"
                + "modifies class M.C {"
                + "adds Int getX() { Int x = 17; return x; }"
                + "}"
        );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        AddMethodModifier mod = (AddMethodModifier) mmod.getModifier(0);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(1);
        assertTrue(stmt.getRetExp() instanceof VarUse);
    }
}
