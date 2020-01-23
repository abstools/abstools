/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typesystem;

import static org.junit.Assert.assertEquals;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.ExpFunctionDef;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ParametricFunctionDecl;
import org.abs_models.frontend.ast.ReturnStmt;
import org.abs_models.frontend.ast.TypeParameterDecl;
import org.abs_models.frontend.typechecker.DataTypeType;
import org.abs_models.frontend.typechecker.KindedName;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.TypeParameter;
import org.abs_models.frontend.typechecker.UnionType;
import org.junit.Test;

public class AnyTypeTests extends FrontendTest {
    @Test
    public void testAssignLitToAny() {
        Model m = assertParse("{ Any x = 42; }");
        assertEquals(m.getAnyType(), getTypeOfFirstVariableDeclaration(m));
    }

    @Test
    public void testAssignAnyToAny() {
        Model m = assertParse("{ Any x = 42; Any y = x; }");

        assertEquals(m.getAnyType(), getTypeOfNthAssignment(m, 2));
        assertEquals(m.getAnyType(), getTypeOfNthVariableDeclaration(m, 2));
    }
}
