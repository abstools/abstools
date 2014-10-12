/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.ast.DataTypeDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.TypeSynDecl;

/**
 * Testing the adding functions, data types and type synonyms using deltas.
 * @author pwong
 *
 */
public class DeltaAddFunctionalTest extends DeltaTest {

    @Test
    public void addFun() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "def Int i() = 1;"
                + "delta I; uses M;"
                + "adds def Int j<A>(A a) = 2;"
                + "adds def Int h() = 2;"
        );
        Decl funI = findDecl(model, "M", "i");
        assertNotNull(funI);
        assertThat(funI, instanceOf(FunctionDecl.class));
        DeltaDecl delta = findDelta(model, "I");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));
        Decl funj = findDecl(model, "M", "j");
        assertNull(funj);
        Decl funh= findDecl(model, "M", "h");
        assertNull(funh);

        model.applyDelta(delta);
        funj = findDecl(model, "M", "j");
        assertNotNull(funj);
        assertThat(funj, instanceOf(FunctionDecl.class));
        funh = findDecl(model, "M", "h");
        assertNotNull(funh);
        assertThat(funh, instanceOf(FunctionDecl.class));
    }
    
    @Test
    public void addDataType() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "data O = O;"
                + "delta I; uses M;"
                + "adds data X<A> = X(A a) | N;"
                + "adds data Y = K | Y(Int i);"
        );
        Decl dataO = findDecl(model, "M", "O");
        assertNotNull(dataO);
        assertThat(dataO, instanceOf(DataTypeDecl.class));
        DeltaDecl delta = findDelta(model, "I");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));
        Decl dataX = findDecl(model, "M", "X");
        assertNull(dataX);
        Decl dataY= findDecl(model, "M", "Y");
        assertNull(dataY);

        model.applyDelta(delta);
        dataX = findDecl(model, "M", "X");
        assertNotNull(dataX);
        assertThat(dataX, instanceOf(ParametricDataTypeDecl.class));
        dataY = findDecl(model, "M", "Y");
        assertNotNull(dataY);
        assertThat(dataY, instanceOf(DataTypeDecl.class));
    }
    
    @Test
    public void addTypeSyn() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "type X = Int;"
                + "delta I; uses M;"
                + "adds type Y = X;"
        );
        Decl typeX = findDecl(model, "M", "X");
        assertNotNull(typeX);
        assertThat(typeX, instanceOf(TypeSynDecl.class));
        DeltaDecl delta = findDelta(model, "I");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));
        Decl typeY= findDecl(model, "M", "Y");
        assertNull(typeY);

        model.applyDelta(delta);
        typeY = findDecl(model, "M", "Y");
        assertNotNull(typeY);
        assertThat(typeY, instanceOf(TypeSynDecl.class));
    }
    
}
