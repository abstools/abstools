/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

import abs.ABSTest.Config;
import abs.common.WrongProgramArgumentException;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.*;
import abs.frontend.typechecker.TypeCheckerHelper;

public class ProductDeclarationTest extends DeltaTest {

    @Test
    public void oldProductSyntax() {
        Model model = assertParseOk(
                "product P1(F1, F2, F3);"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P1");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }
        ImplicitProduct impl = p.getImplicitProduct();
        assertEquals(3, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void productFeatureSet() {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3};"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P1");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }
        ImplicitProduct impl = p.getImplicitProduct();
        assertEquals(3, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void productUnion() {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3} || {F4};"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P1");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }
        ImplicitProduct impl = p.getImplicitProduct();
        assertEquals(4, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F2", "F3", "F4"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void productIntersect() {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3} && {F2, F3};"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P1");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }
        ImplicitProduct impl = p.getImplicitProduct();
        assertEquals(2, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F2", "F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void productName() {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3};"
                        + "product P2 = P1;"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P2");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }
        ImplicitProduct impl = p.getImplicitProduct();
        assertEquals(3, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test(expected=DeltaModellingException.class)
    public void cylicProduct() {
        Model model = assertParseOk(
                "product P4 = P5;"
                        + "product P5 = P4;"
                );
        model.evaluateAllProductDeclarations();
    }

    @Test
    public void undeclaredProduct() {
        Model model = assertParseOk(
                "product P1 = P2 && P3 || P4 || {F1, F2};"
                );

        ProductDecl p = null;
        try {
            p = model.findProduct("P1");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }

        SemanticErrorList e = new SemanticErrorList();        
        typeCheck(model, p, e);

        // Four errors here:
        // three undeclared products (P2, P3, P4)
        // and P1 is invalid because there is no feature model declared
        assertEquals(4, e.size());
        assertEquals(ErrorMessage.UNDECLARED_PRODUCT, e.getFirst().msg);
    }

    @Test
    public void invalidProduct() {
        Model model = assertParseOk(
                "product P1 = {F1, F2};"
                        + "root FM {"
                        + "group allof { F1, F2, F3 }"
                        + "}"                
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P1");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }

        SemanticErrorList e = new SemanticErrorList();        
        typeCheck(model, p, e);
        
        assertEquals(1, e.size());
        assertEquals(ErrorMessage.INVALID_PRODUCT, e.getFirst().msg);
    }

    @Test
    public void complexExpression() {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3} && {F3, F4} || {F5};"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P1");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }
        ImplicitProduct impl = p.getImplicitProduct();
        assertEquals(2, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F3", "F5"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void complexExpressionWithParenthesis() {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3} && ({F3, F4} || {F5});"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P1");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }
        ImplicitProduct impl = p.getImplicitProduct();
        assertEquals(1, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void rightSideProductName() {
        Model model = assertParseOk("product P1 = P2 && P3 || P4 || {F1, F2};");

        ProductDecl p = null;
        try {
            p = model.findProduct("P1");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }

        Set<String> actual = new HashSet<String>();
        p.getProductExpr().setAllProductName(actual);
        assertEquals(3, actual.size());

        Set<String> expected = new HashSet<String>(Arrays.asList("P2", "P3", "P4"));
        assertEquals(expected, actual);
    }

    @Test
    public void unorderedProduct() {
        Model model = assertParseOk(
                "product P2 = P1;"
                        + "product P3 = P2;"
                        + "product P1 = {F1, F2, F3};"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P3");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }
        ImplicitProduct impl = p.getImplicitProduct();
        assertEquals(3, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }
}
