/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typesystem;

import org.abs_models.frontend.analyser.SemanticCondition;
import org.junit.Test;

import org.abs_models.frontend.FrontendTest;

import static org.abs_models.ABSTest.Config.*;

public class ModuleSystemTests extends FrontendTest {

    @Test
    public void simpleModule() {
        assertTypeOK("module A;");
    }

    @Test
    public void simpleModule2() {
        assertTypeOK("module A; module B;");
    }

    @Test
    public void simpleModule3() {
        assertTypeOK("module A; data X; module B;");
    }

    @Test
    public void qualifiedImport() {
        assertTypeOK("module A; export Foo, Bar; data Foo = Bar; module B; import A.Foo; import A.Bar; { A.Foo f = A.Bar; } ");
    }

    @Test
    public void qualifiedUnqualifiedImport() {
        assertTypeOK("module Definitions; export *;"
                     + "class A {} class B {}"
                     + "module Importer;"
                     + "import A from Definitions; import Definitions.B;"
                     + "{"
                     + "    new A();"
                     + "    new Definitions.B();"
                     + "}"
                     );
    }

    @Test
    public void starImport() {
        assertTypeOK("module A; export X; data X; module B; import * from A; type Y = X;");
    }

    @Test
    public void exportedImport() {
        assertTypeOK("module A; export X; data X; module B; import A.X; type Y = A.X;");
    }

    @Test
    public void starExportedImport() {
        assertTypeOK("module A; export X; data X; module B; export * from A; import A.X; module C; import * from B; type Y = B.X;");
    }

    @Test
    public void fromExportedImport() {
        assertTypeOK("module A; export X; data X; module B; export X from A; import A.X; module C; import * from B; type Y = B.X;");
    }

    @Test
    public void exportQualified() {
        assertTypeOK("module A; export A.X; data X; module B; import X from A; type Y = X;");
    }

    @Test
    public void fromImport() {
        assertTypeOK("module A; export X; data X; module B; import X from A; type Y = X;");
    }

    @Test
    public void fromImportQualifiedUse() {
        assertTypeOK("module A; export X; data X; module B; import X from A; type Y = A.X;");
    }

    @Test
    public void exportImportedName() {
        assertTypeOK("module A; export X; data X; module B; export X from A; import * from A;  ");
    }

    @Test
    public void exportImportedName2() {
        assertTypeOK("module A; export X; data X; module B; export X from A; import X from A;  ");
    }

    @Test
    public void starExportImportedNames() {
        assertTypeOK("module A; export *; data X; module B; export * from A; import * from A; module C; import * from B; type Y = X; ");
    }

    @Test
    public void starExportImportedNames2() {
        assertTypeOK("module A; export *; data X; module E; export *; data Z; module B; export * from E; export * from A; import * from A; import * from E; module C; import * from B; type Y = X; ");
    }

    @Test
    public void exportImportSwapped() {
        // test the relaxed order of import and export clauses
        assertTypeOK("module A; export *; data Foo; data Bar; module B; export Baz; import Foo from A; export Glunk; import Bar from A; type Baz = Foo; type Glunk = Bar;");
    }
    // NEGATIVE TESTS

    @Test
    public void unqualifiedImport() {
        assertTypeErrors("module A; data X; module B; import X; ");
    }

    @Test
    public void importFromNotExistingModule() {
        assertTypeErrors("module B; import A.X; ");
    }

    @Test
    public void notExportedImport() {
        assertTypeErrors("module A; data X; module B; import A.X; ");
    }

    @Test
    public void notExportedImport2() {
        assertTypeErrors("module A; data X; module B; import A.X; type Y = X;");
    }

    @Test
    public void notExportedImportFrom() {
        assertTypeErrors("module A; data X; module B; import X from A; ");
    }

    @Test
    public void notExportedImportFrom2() {
        assertTypeErrors("module A; export X; data X; data Y; module B; import X, Y from A; ");
    }

    @Test
    public void starImportNotExistingModule() {
        assertTypeErrors("module B; import * from A; ");
    }

    @Test
    public void qualifiedImportUnqualifiedUse() {
        assertTypeErrors("module A; export X; data X; module B; import A.X; type Y = X; ");
    }

    @Test
    public void qualifiedFromImport() {
        assertTypeErrors("module A; export X; data X; module B; import A.X from A; ");
    }

    @Test
    public void invisibleExport() {
        assertTypeErrors("module A; export X; ");
    }

    @Test
    public void exportFromNotExistingModule() {
        assertTypeErrors("module A; export X from B; ");
    }

    @Test
    public void exportNotImportedName() {
        assertTypeErrors("module A; export X; data X; data Y; module B; export Y from A; import * from A;  ");
    }

    @Test
    public void invisibleExportFrom() {
        assertTypeErrors("module A; export X from B; ");
    }
    
    @Test
    public void ambigiousUseInterface() {
        // see bug #271, the reference to interface I is ambigious and should not compile
        assertTypeErrors("module A; export I; interface I{}" +
        		"module B; export I; interface I{}" +
        		"module C; import I from A; import I from B; class C implements I {} ");
    }
    
    @Test
    public void ambigiousUseInterfaceFix() {
        // same as above but with qualified use. This should work
        assertTypeOK("module A; export I; interface I{}" +
                        "module B; export I; interface I{}" +
                        "module C; import I from A; import I from B; class C implements A.I {} ");
    }
    
    @Test
    public void ambigiousUseData() {
        assertTypeErrors("module A; export I; data I = I;" +
                        "module B; export I; data I = I;" +
                        "module C; import I from A; import I from B; data J = J(I mmy); ");
    }
    
    @Test
    public void ambigiousUseData2() {
        assertTypeErrors("module A; export I; data I = I;" +
                        "module B; export I; data I = I;" +
                        "module C; import I from A; import I from B; data I = I; ");
    }
    
    @Test
    public void ambigiousUseClass() {
        assertTypeErrors("module A; export *; interface I {} class K implements I {}" +
                     "module B; export *; interface J {} class K implements J {}" +
                     "module C; import * from A; import * from B; { I k = new local K(); } ");
    }
    
    @Test
    public void ambigiousUseFunction() {
        assertTypeErrors("module A; export *; def Int foo() = 3;" +
                     "module B; export *; def Int foo() = 4;" +
                     "module C; import * from A; import * from B; {Int x = foo(); } ");
    }

    @Test
    public void module_duplicate_definition() throws Exception {
        // don't silently overwrite imported functions etc. 
        // https://github.com/abstools/abstools/issues/144
        assertTypeErrors("module Test2; export *; def Int foo() = 42; module Test; import * from Test2; def Int foo() = 24;", Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void shadowImportedNames() {
        // see bug #271
        // the definition of interface I in B should shadow the imported interface 
        assertTypeErrors("module A; export I;\n" +
        		"interface I{ Unit a(); }\n" +
        		"\n" +
                        "module B; import I from A; \n" +
                        "interface I { Unit b();}\n" +
                        "class C implements I { Unit b() {} } \n");
    }
    
    @Test 
    public void importSameDefinitionMultipleTimes() {
        // Interface I is imported via M2 and via M3 but as it is the same interface this should compile
        assertTypeOK(
                    "module M;" +
                    "export *;" +
                    "interface I {}" +
                    
                    "module M2;" +
                    "import I from M;" +
                    "export I from M;" +
                    
                    "module M3;" +
                    "import I from M;" +
                    "export I from M;" +
                    
                    "module Test;" +
                    "import * from M2;" +
                    "import * from M3;" +
                    
                    "class C implements I {} ");
    }

    @Test
    public void importTransitiveStar() {
        assertTypeOK("module M1; export *; class A {}"
        + "module M2; import * from M1; export * from M1;"
        + "module M3; import * from M2; { new A(); }");
    }

    @Test
    public void importTransitiveNamed() {
        assertTypeOK("module M1; export A; class A {}"
            + "module M2; import A from M1; export A from M1;"
            + "module M3; import A from M2; { new A(); }");
    }


    @Test
    public void selfImport() {
        // see bug #94
        assertTypeOK("module M; export X from M; import X from M; data X;");
    }
    
    @Test
    public void circularDefinition() {
        assertTypeErrors("module A; export X; import X from B; module B; export X; import X from A; ");
    }

    @Test
    public void importPartialStdLib() {
        assertTypeErrors("module A; import Unit from ABS.StdLib;\n" +
                 "{ println(\"println not imported and hence not accessible\"); } \n");

    }
}
