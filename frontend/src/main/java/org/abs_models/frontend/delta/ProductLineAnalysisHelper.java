/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.analyser.SemanticError;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.mtvl.ChocoSolver;

import choco.kernel.model.constraints.Constraint;
import org.checkerframework.checker.units.qual.C;

public class ProductLineAnalysisHelper {

    /*
     * An IFΔJ product line is type-safe if the following conditions hold:
     * (i) it is strongly unambiguous,
     * (ii) its product generation mapping is total, and
     * (iii) all its products are well-typed IFJ programs.
     */
    public static void typeCheckPL(ProductLine pl, SemanticConditionList errors) {
        assert pl != null;

        if (! wellFormedProductLine(pl, errors))
            return;

        long time0 = System.currentTimeMillis();
        // Check strong unambiguity
        checkStrongUnambiguity(pl, errors);
        // Build the product family generation trie. Hereby check that
        // - product generation mapping is total
        // - TODO all products are well-typed programs
        DeltaTrie pfgt = buildPFGT(pl, errors);

        if (pl.getModel().debug) {
            //System.out.println("Trie height:\n" + pfgt.height());
            //System.out.println("Trie:\n" + pfgt.toString());
            long time1 = System.currentTimeMillis();
            System.out.println("\u23F1 Type checking SPL duration (s): " + ((time1 - time0)/1000.0));
        }

    }


    /*
     * Check that the 'productline' declaration is well formed. This means...
     * - after clauses do not reference any deltaIDs that do not have their own delta clause
     * - deltas named in the productline correspond to actual DeltaDecls
     */
    protected static boolean wellFormedProductLine(ProductLine pl, SemanticConditionList e) {
        boolean wellformed = true;
        boolean local = pl instanceof LocalProductLine;

        // preliminaries
        final Set<String> declaredDeltas = local ? ((LocalProductLine) pl).getDeltaDeclsMap().keySet() :
        pl.getModel().getDeltaDeclsMap().keySet();
        final Set<String> referencedDeltas = new HashSet<>(pl.getDeltaClauses().getNumChild());
        for (DeltaClause clause : pl.getDeltaClauses())
            referencedDeltas.add(clause.getDeltaspec().getDeltaID());

        // check
        for (DeltaClause clause : pl.getDeltaClauses()) {
            // ensure deltas in the productline correspond to actual DeltaDecls
            if (!declaredDeltas.contains(clause.getDeltaspec().getDeltaID())) {
                e.add(new SemanticError(clause, ErrorMessage.NO_DELTA_DECL, clause.getDeltaspec().getDeltaID()));
                wellformed = false;
            }
            // ensure 'after' clauses do not reference any deltaIDs that do not have their own delta clause
            for (DeltaID id : clause.getAfterDeltaIDs()) {
                String afterID = id.getName();
                if (!referencedDeltas.contains(afterID)) {
                    e.add(new SemanticError(clause, ErrorMessage.MISSING_DELTA_CLAUSE_ERROR, afterID,
                        pl instanceof LocalProductLine ? pl.getModuleDecl().getName() : pl.getName()));
                    wellformed = false;
                }
            }
        }
        return wellformed;
    }


    public static DeltaTrie buildPFGT(ProductLine pl, SemanticConditionList errors) {
        Model model = pl.getModel();
        DeltaTrie trie = new DeltaTrie(model, errors);

        for (Product product : model.getProductList()) {
            // for each product: obtain sequence of deltas & add it to the trie
            Set<String> applicableDeltas = pl.findApplicableDeltas(product);
            List<String> productGenerationString = pl.sortDeltas(applicableDeltas);
            trie.addWord(productGenerationString, product);
        }
        return trie;
    }

    public static void checkStrongUnambiguity(ProductLine pl, SemanticConditionList l) {
        isStronglyUnambiguous(pl, l);
    }

    /*
     * A product line is strongly unambiguous if each set in the partition of
     * the delta modules specified in the product-line declaration is
     * consistent, that is, if one delta module in a set adds or removes a
     * class, no other delta module in the same set may add, remove or modify
     * the same class, and the modifications of the same class in different
     * delta modules in the same set have to be disjoint.
     */
    public static boolean isStronglyUnambiguous(ProductLine pl, SemanticConditionList errors) {

        boolean result = true;
        Model model = pl.getModel();
        /*
        System.out.print("Delta partition: ");
        for (Set<String> set : pl.getDeltaPartition()) {
            System.out.print("{");
            for (String el : set)
                System.out.print(" " + el + " ");
            System.out.print("}   ");
        }
        System.out.println();
         */
        assert pl.getDeltaPartition() != null;
        for (Set<String> set : pl.getDeltaPartition()) {

            // Remember the names of classes and methods modified by deltas in
            // current set
            // { Module.Class -> { Method -> Delta } }
            // { Module.Class -> { "CLASS" -> Delta } }
            Map<String, Map<String, String>> cache = new HashMap<>();

            for (String deltaID : set) {
                // assumes the DeltaDecl corresponding to deltaID exists (wellFormedProductLine)
                DeltaDecl delta = pl instanceof LocalProductLine ? ((LocalProductLine) pl).getDeltaDeclsMap().get(deltaID) :
                    model.getDeltaDeclsMap().get(deltaID);

                assert delta.getModuleModifiers() != null;
                for (ModuleModifier moduleModifier : delta.getModuleModifiers()) {
                    if (moduleModifier instanceof ModifyClassModifier) {
                        //String methodID;
                        String prefix = ((ClassModifier) moduleModifier).getQualifiedName();

                        for (Modifier mod : ((ModifyClassModifier) moduleModifier).getModifiers()) {
                            /*if (mod instanceof AddMethodModifier)
                                methodID = ((AddMethodModifier) mod).getMethodImpl().getMethodSig().getName();
                            else if (mod instanceof RemoveMethodModifier)
                                methodID = ((RemoveMethodModifier) mod).getMethodSig().getName();
                            else if (mod instanceof ModifyMethodModifier)
                                methodID = ((ModifyMethodModifier) mod).getMethodImpl().getMethodSig().getName();
                            else
                                continue;*/

                            if (mod instanceof DeltaTraitModifier) {
                                HashSet<String> methodIDSet = new HashSet<>();
                                ((DeltaTraitModifier) mod).collectMethodIDs(methodIDSet, model);

                                for (String methodID : methodIDSet) {
                                    if (cache.containsKey(prefix)) {
                                        if (cache.get(prefix).containsKey(methodID)) {
                                            result = false;
                                            String otherDeltaID = cache.get(prefix).get(methodID);
                                            if (! deltaID.equals(otherDeltaID))
                                                errors.add(new TypeError(pl, ErrorMessage.AMBIGUOUS_PRODUCTLINE, pl instanceof LocalProductLine? "in " + pl.getModuleDecl().getName() : pl.getName() , deltaID, otherDeltaID, prefix + ", method " + methodID));
                                            else
                                                ; // FIXME also a kind of ambiguity but needs a different error message
                                        } else if (cache.get(prefix).containsKey("CLASS")) {
                                            result = false;
                                            String otherDeltaID = cache.get(prefix).get("CLASS");
                                            if (! deltaID.equals(otherDeltaID))
                                                errors.add(new TypeError(pl, ErrorMessage.AMBIGUOUS_PRODUCTLINE, pl instanceof LocalProductLine? "in " + pl.getModuleDecl().getName() : pl.getName() , deltaID, otherDeltaID, prefix));
                                            else
                                                ; // FIXME also a kind of ambiguity but needs a different error message
                                        } else {
                                            cache.get(prefix).put(methodID, deltaID);
                                        }
                                    } else {
                                        cache.put(prefix, new HashMap<>());
                                        cache.get(prefix).put(methodID, deltaID);
                                    }
                                }
                            }
                        }
                    } else if (moduleModifier instanceof AddClassModifier
                            || moduleModifier instanceof RemoveClassModifier) {

                        String prefix = ((ClassModifier) moduleModifier).getQualifiedName();
                        if (cache.containsKey(prefix)) {
                            result = false;
                            assert ! cache.get(prefix).isEmpty();
                            String otherDeltaID = cache.get(prefix).values().iterator().next();
                            errors.add(new TypeError(pl, ErrorMessage.AMBIGUOUS_PRODUCTLINE, pl.getName(), deltaID, otherDeltaID, prefix));
                            System.out.println("3 ambiguity due to " + deltaID + "<>" + otherDeltaID);
                        } else {
                            cache.put(prefix, new HashMap<>());
                            cache.get(prefix).put("CLASS", deltaID);
                        }
                    }
                }
                // TODO apply same reasoning to other elements: fields,
                // functions, ADTs, etc
            }
        }
        // TODO remove boolean result unless needed
        return result;
    }

    /*
     * Build all SPL configurations (valid feature selections, ignoring attributes), one by one
     * The purpose is to measure how long this takes, so we can compare it with the performance of type checking the SPL.
     *
     */
    public static void buildAndPrintAllConfigurations(Model m) {

        long timeSum = 0;
        for (Product product : m.getProductList()) {

            long time0 = System.currentTimeMillis();
            System.out.println("\u23F1 Flattening product: " + product.getFeatureSetAsString());

            // Find a solution to the feature model that satisfies the product feature selection
            ChocoSolver s = m.instantiateCSModel();
            HashSet<Constraint> newcs = new HashSet<>();
            product.getProdConstraints(s.vars, newcs);
            for (Constraint c: newcs)
                s.addConstraint(c);

            Map<String, Integer> solution = s.getSolution();
            System.out.println("\u23F1 Full product configuration: " + solution);
            long time1 = System.currentTimeMillis();

            // map the solution to the product,
            // i.e. add attribute assignments to features
            for (String fname : solution.keySet()) {
                if (fname.startsWith("$")) // ignore internal ChocoSolver variable
                    continue;
                if (fname.contains(".")) {
                    String[] parts = fname.split("\\.");
                    String fid = parts[0];
                    String aid = parts[1];
                    Integer val = solution.get(fname);
                    for (Feature feature : product.getFeatures()) {
                        if (feature.getName().equals(fid)) {
                            feature.addAttrAssignment(new AttrAssignment(aid, new IntVal(val)));
                            break;
                        }
                    }
                }
            }

            long time2 = System.currentTimeMillis();

            Model thisModel = m.treeCopyNoTransform();

            long time3 = System.currentTimeMillis();
            thisModel.flattenForProduct(product);

            long time4 = System.currentTimeMillis();
            timeSum += (time4 - time3);
            System.out.println("\u23F1 Time: " + (time1 - time0) + " | " + (time2 - time1) + " | " + (time3 - time2) + " | " + (time4 - time3) + " | " + "Total(s): " + ((time4 - time0)/1000.0));
        }
        System.out.println("\u23F1 Flattening total time (s): " + timeSum/1000.0);
    }

}
