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
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.*;
import abs.frontend.typechecker.TypeCheckerHelper;

public class ProductDeclarationTest extends DeltaTest {

    @Test
    public void oldProductSyntax() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1(F1, F2, F3);"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P1");

        Product impl = p.getProduct();
        assertEquals(3, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void productFeatureSet() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3};"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P1");

        Product impl = p.getProduct();
        assertEquals(3, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void productIntersect() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3} && {F2, F3};"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P1");

        Product impl = p.getProduct();
        assertEquals(2, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F2", "F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void productUnion() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3} || {F4};"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P1");

        Product impl = p.getProduct();
        assertEquals(4, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F2", "F3", "F4"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void productDifference() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3, F4} - {F2, F3};"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P1");

        Product impl = p.getProduct();
        assertEquals(2, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F4"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void productName() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3};"
                        + "product P2 = P1;"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P2");

        Product impl = p.getProduct();
        assertEquals(3, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test(expected=DeltaModellingException.class)
    public void cylicProduct() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P4 = P5;"
                        + "product P5 = P4;"
                );

        model.evaluateAllProductDeclarations();
    }

    @Test
    public void undeclaredProduct() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = P2 && P3 || P4 || {F1, F2};"
                );

        ProductDecl p = model.findProduct("P1");

        SemanticConditionList e = new SemanticConditionList();
        typeCheck(model, p, e);

        assertEquals(3, e.getErrorCount());
        assertEquals(ErrorMessage.UNDECLARED_PRODUCT, e.getFirstError().msg);
    }

    @Test
    public void validProduct() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3};"
                        + "root FM {"
                        + "group allof { F1, F2, F3 }"
                        + "}"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P1");

        SemanticConditionList e = new SemanticConditionList();
        typeCheck(model, p, e);

        assertEquals(0, e.getErrorCount());
    }

    @Test
    public void invalidProduct() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = {F1, F2};"
                        + "root FM {"
                        + "group allof { F1, F2, F3 }"
                        + "}"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P1");

        SemanticConditionList e = new SemanticConditionList();
        typeCheck(model, p, e);

        assertEquals(1, e.getErrorCount());
        assertEquals(ErrorMessage.INVALID_PRODUCT, e.getFirstError().msg);
    }

    @Test
    public void complexExpression() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3} && {F3, F4} || {F5, F6} - {F6};"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P1");

        Product impl = p.getProduct();
        assertEquals(2, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F3", "F5"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void complexExpressionWithParenthesis() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3} && ({F3, F4} || {F5}) || {F6} - {F6};"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P1");

        Product impl = p.getProduct();
        assertEquals(1, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void rightSideProductName() throws WrongProgramArgumentException {
        Model model = assertParseOk("product P1 = P2 && P3 || P4 || {F1, F2};");

        ProductDecl p = model.findProduct("P1");

        Set<String> actual = new HashSet<String>();
        p.getProductExpr().setRightSideProductNames(actual);
        assertEquals(3, actual.size());

        Set<String> expected = new HashSet<String>(Arrays.asList("P2", "P3", "P4"));
        assertEquals(expected, actual);
    }

    @Test
    public void unorderedProduct() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P2 = P1;"
                        + "product P3 = P2;"
                        + "product P1 = {F1, F2, F3};"
                );

        model.evaluateAllProductDeclarations();
        ProductDecl p = model.findProduct("P3");

        Product impl = p.getProduct();
        assertEquals(3, impl.getNumFeature());

        Set<String> expected = new HashSet<String>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<String>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }
}
