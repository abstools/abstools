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
    public void featureSet1() {
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
    public void featureSet2() {
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
    public void union1() {
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
    public void union2() {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3};"
                        + "product P2 = {F4};"
                        + "product P3 = P1 || P2;"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P3");
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
    public void intersect1() {
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
    public void intersect2() {
        Model model = assertParseOk(
                "product P1 = {F1, F2, F3};"
                        + "product P2 = {F2, F3};"
                        + "product P3 = P1 && P2;"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P3");
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

    @Test(expected=DeltaModellingException.class)
    public void cylic() {
        Model model = assertParseOk(
                "product P4 = P5;"
                        + "product P5 = P4;"
                );
        model.evaluateAllProductDeclarations();
    }

    @Test
    public void undeclaredProduct() {
        Model model = assertParseOk("product P1 = P2 && P3 || P4;");

        ProductDecl p = null;
        try {
            p = model.findProduct("P1");
        } catch (WrongProgramArgumentException e) {
            e.printStackTrace();
        }

        SemanticErrorList e = new SemanticErrorList();        
        typeCheck(model, p, e);

        assertEquals(3, e.size());
        assertEquals(ErrorMessage.UNDECLARED_PRODUCT, e.getFirst().msg);
    }

    @Test
    public void parenthesis1(){
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
    public void parenthesis2(){
        Model model = assertParseOk(
                "product P1(F1, F2, F3);"
                        + "product P2(F3, F4);"
                        + "product P3(F5);"
                        + "product P4 = P1 && (P2 || P3);"
                );
        model.evaluateAllProductDeclarations();

        ProductDecl p = null;
        try {
            p = model.findProduct("P4");
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
    public void complexExpression() {
        Model model = assertParseOk(
                "product P1(F1, F2, F3);"
                        + "product P2(F3, F4);"
                        + "product P3(F5);"
                        + "product P4 = P1 && P2;"
                        + "product P5 = P1 || P2;"
                        + "product P6 = P1 && P2 || P3 || {F7, F8} || P5;"
                        + "product P7 = P1 && (P2 || P3) || {F7, F8} || P5;"
                );

        HashMap<String, Set<String>> products = new HashMap<String, Set<String>>();
        products.put("P1", new HashSet<String>(Arrays.asList("F1", "F2", "F3")));
        products.put("P2", new HashSet<String>(Arrays.asList("F3", "F4")));
        products.put("P3", new HashSet<String>(Arrays.asList("F5")));
        products.put("P4", new HashSet<String>(Arrays.asList("F3")));
        products.put("P5", new HashSet<String>(Arrays.asList("F1", "F2", "F3", "F4")));
        products.put("P6", new HashSet<String>(Arrays.asList("F1", "F2", "F3", "F4", "F5", "F7", "F8")));
        products.put("P7", new HashSet<String>(Arrays.asList("F1", "F2", "F3", "F4", "F7", "F8")));

        model.evaluateAllProductDeclarations();
        for (ProductDecl p : model.getProductDecls()) {
            Set<String> actual = new HashSet<String>();
            for(Feature f : p.getImplicitProduct().getFeatures()){
                actual.add(f.getName());
            }
            Set<String> expected = products.get(p.getName());

            assertEquals(expected, actual);
        }
    }

}
