/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;

import org.junit.Test;

import abs.frontend.ast.*;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.KindedName.Kind;
import abs.frontend.typechecker.UnknownType;

public class VarResolutionTest extends DeltaTest {

    @Test
    public void fromAddFieldModifierToCoreTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C {"
                + "Int x = 0;"
                + "}"
                + "delta D; uses M;"
                + "modifies class C {"
                + "adds Int y = x;"
                + "}"
        );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        AddFieldModifier mod = (AddFieldModifier) mmod.getModifier(0);
        // until a delta is applied, we may not be able to tell vars from fields:
        assertThat(mod.getFieldDecl().getInitExp() , instanceOf(VarUse.class));
        assertEquals(UnknownType.INSTANCE,mod.getFieldDecl().getInitExp().getType());
    }

    @Test
    public void fromAddFieldModifierToCoreTest2() {
        Model model = assertParseOk(
                "module M;"
                + "class C {"
                + "Int x = 0;"
                + "}"
                + "delta D;uses M;"
                + "modifies class C {"
                + "adds Int y = x;"
                + "}"
        );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        mmod.apply();
        ModuleDecl m = model.lookupModule("M");
        ClassDecl c = (ClassDecl) m.lookup(new KindedName(Kind.CLASS, "C"));
        FieldDecl fy = (FieldDecl) c.locallookupVarOrFieldName("y", true);
        assertThat(fy.getInitExp() , instanceOf(FieldUse.class));
    }

    @Test
    public void fromAddMethodModifierToCoreTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C {"
                + "Int x = 0;"
                + "}"
                + "delta D;uses M;"
                + "modifies class C {"
                + "adds Int getX() { return x; }"
                + "}"
        );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        AddMethodModifier mod = (AddMethodModifier) mmod.getModifier(0);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(0);
        assertThat(stmt.getRetExp(), instanceOf(FieldUse.class));
    }

    @Test
    public void fromModifyMethodModifierToCoreTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C {"
                + "Int x = 0;"
                + "Int getX() { }"
                + "}"
                + "delta D;uses M;"
                + "modifies class C {"
                + "modifies Int getX() { return x; }"
                + "}"
                );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        ModifyMethodModifier mod = (ModifyMethodModifier) mmod.getModifier(0);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(0);
        assertThat(stmt.getRetExp(), instanceOf(FieldUse.class));
    }
    
    @Test
    public void fromModifierToSameClassModifierTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C { }"
                + "delta D;uses M;"
                + "modifies class C {"
                + "adds Int x = 0;"
                + "adds Int getX() { return x; }"
                + "}"
                );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        AddMethodModifier mod = (AddMethodModifier) mmod.getModifier(1);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(0);
        assertThat(stmt.getRetExp(), instanceOf(FieldUse.class));
    }
    
    @Test
    public void fromModifierToOtherModifyClassModifierTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C { }"
                + "delta D;"
                + "uses M;"
                + "modifies class C {"
                + "adds Int x = 0;"
                + "}"
                + " modifies class C {"
                + "adds Int getX() { return x; }"
                + "}"
                );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(1);
        AddMethodModifier mod = (AddMethodModifier) mmod.getModifier(0);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(0);
        assertThat(stmt.getRetExp(), instanceOf(FieldUse.class));
    }

    @Test
    public void fromModifierToOtherAddClassModifierTest() {
        Model model = assertParseOk(
                "module M;"
                + "delta D;"
                + "uses M;"
                + "adds class C {"
                + "Int x = 0;"
                + "}"
                
                + "modifies class C {"
                + "adds Int getX() { return x; }"
                + "}"
                );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(1);
        AddMethodModifier mod = (AddMethodModifier) mmod.getModifier(0);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(0);
        assertThat(stmt.getRetExp(), instanceOf(FieldUse.class));
    }

    @Test
    public void nonFieldTest() {
        Model model = assertParseOk(
                "module M;"
                + "class C {"
                + "Int x = 0;"
                + "}"
                + "delta D;"
                + "uses M;"
                + "modifies class C {"
                + "adds Int getX() { Int x = 17; return x; }"
                + "}"
        );
        DeltaDecl delta = findDelta(model, "D");
        ModifyClassModifier mmod = (ModifyClassModifier) delta.getModuleModifier(0);
        AddMethodModifier mod = (AddMethodModifier) mmod.getModifier(0);
        ReturnStmt stmt = (ReturnStmt) mod.getMethodImpl().getBlock().getStmt(1);
        assertThat(stmt.getRetExp(), instanceOf(VarUse.class));
    }
    
    @Test
    public void defUseMultipleFiles() throws Exception {
    Model m = this.assertParseFilesOk(new HashSet<String>() {{ add("tests/abssamples/deltas/defuse/def.abs"); add("tests/abssamples/deltas/defuse/use.abs");}}, true);
    m.evaluateAllProductDeclarations();  
    m.flushCache();
    m.flattenForProduct("Prod"); 
    m.flushCache();
    assertTrue(!m.typeCheck().containsErrors());
    }
}
