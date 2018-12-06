/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.Feature;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.Product;
import org.abs_models.frontend.ast.ProductDecl;
import org.junit.Assert;
import org.junit.Test;

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

        Set<String> expected = new HashSet<>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<>();
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

        Set<String> expected = new HashSet<>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<>();
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

        Set<String> expected = new HashSet<>(Arrays.asList("F2", "F3"));
        Set<String> actual = new HashSet<>();
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

        Set<String> expected = new HashSet<>(Arrays.asList("F1", "F2", "F3", "F4"));
        Set<String> actual = new HashSet<>();
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

        Set<String> expected = new HashSet<>(Arrays.asList("F1", "F4"));
        Set<String> actual = new HashSet<>();
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

        Set<String> expected = new HashSet<>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    // FIXME: there's a catch clause somewhere throwing a RuntimeException
    // after catching the DeltaModellingException.  The proper fix is to just
    // add an error to the SemanticConditionList during type-checking instead
    // of throwing an exception
    @Test(expected=java.lang.RuntimeException.class)
    // @Test(expected=DeltaModellingException.class)
    public void cylicProduct() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P4 = P5;"
                        + "product P5 = P4;"
                );

        model.evaluateAllProductDeclarations();
    }

    // FIXME: The correct error message is emitted, but via throwing an
    // exception instead of reporting it in the condition list.  Additionally,
    // the WrongProgramArgumentException is re-thrown as a RuntimeException.
    // The proper fix is to just add an error to the SemanticConditionList
    // instead of throwing an exception

    // @Test(expected=org.abs_models.common.WrongProgramArgumentException.class)
    @Test(expected=java.lang.RuntimeException.class)
    public void undeclaredProduct() throws WrongProgramArgumentException {
        Model model = assertParseOk(
                "product P1 = P2 && P3 || P4 || {F1, F2};"
                );

        ProductDecl p = model.findProduct("P1");

        SemanticConditionList e = new SemanticConditionList();
        typeCheck(model, p, e);

        assertEquals(3, e.getErrorCount());
        Assert.assertEquals(ErrorMessage.UNDECLARED_PRODUCT, e.getFirstError().msg);
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

        Set<String> expected = new HashSet<>(Arrays.asList("F3", "F5"));
        Set<String> actual = new HashSet<>();
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

        Set<String> expected = new HashSet<>(Arrays.asList("F3"));
        Set<String> actual = new HashSet<>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }

    @Test
    public void rightSideProductName() throws WrongProgramArgumentException {
        Model model = assertParseOk("product P2(); product P3(); product P4(); product P1 = P2 && P3 || P4 || {F1, F2};");

        ProductDecl p = model.findProduct("P1");

        Set<String> actual = new HashSet<>();
        p.getProductExpr().setRightSideProductNames(actual);
        assertEquals(3, actual.size());

        Set<String> expected = new HashSet<>(Arrays.asList("P2", "P3", "P4"));
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

        Set<String> expected = new HashSet<>(Arrays.asList("F1", "F2", "F3"));
        Set<String> actual = new HashSet<>();
        for (Feature f : impl.getFeatures())
            actual.add(f.getName());
        assertEquals(expected, actual);
    }
}
