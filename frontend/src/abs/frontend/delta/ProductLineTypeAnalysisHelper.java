/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.*;

public class ProductLineTypeAnalysisHelper {

    /*
     * An IFΔJ product line is type-safe if the following conditions hold:
     * (i) it is strongly unambiguous,
     * (ii) its product generation mapping is total, and
     * (iii) all its products are well-typed IFJ programs.
     */
    public static void typeCheckPL(ProductLine pl, SemanticConditionList errors) {

        assert pl != null;

        if (wellFormedProductLine(pl, errors)) {
            // Check strong unambiguity
            checkStrongUnambiguity(pl, errors);

            // Build the product family generation trie. Hereby check that
            // - product generation mapping is total
            // - TODO all products are well-typed programs
            DeltaTrie pfgt = buildPFGT(pl, errors);

            // System.out.println(pfgt);
        }
    }

    /*
     * Check that the 'productline' declaration is well formed. This means...
     * - after clauses do not reference any deltaIDs that do not have their own delta clause
     * - deltas named in the productline correspond to actual DeltaDecls
     */
    protected static boolean wellFormedProductLine(ProductLine pl, SemanticConditionList e) {
        boolean wellformed = true;

        // preliminaries
        final Set<String> declaredDeltas = pl.getModel().getDeltaDeclsMap().keySet();
        final Set<String> referencedDeltas = new HashSet<String>(pl.getDeltaClauses().getNumChild());
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
                    e.add(new SemanticError(clause, ErrorMessage.MISSING_DELTA_CLAUSE_ERROR, afterID, pl.getName()));
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
    public static boolean isStronglyUnambiguous(ProductLine pl, SemanticConditionList l) {

        boolean result = true;
        Model model = pl.getModel();

        //        System.out.print("Delta partition: ");
        //        for (Set<String> set : pl.getDeltaPartition()) {
        //            System.out.print("{");
        //            for (String el : set)
        //                System.out.print(" " + el + " ");
        //            System.out.print("}   ");
        //        }
        //        System.out.println();

        assert pl.getDeltaPartition() != null;
        for (Set<String> set : pl.getDeltaPartition()) {

            // Remember the names of classes and methods modified by deltas in
            // current set
            // { Module.Class -> { Method -> Delta } }
            // { Module.Class -> { "CLASS" -> Delta } }
            Map<String, Map<String, String>> cache = new HashMap<String, Map<String, String>>();

            for (String deltaID : set) {
                // assumes the DeltaDecl corresponding to deltaID exists (wellFormedProductLine)
                DeltaDecl delta = model.getDeltaDeclsMap().get(deltaID);

                assert delta.getModuleModifiers() != null;
                for (ModuleModifier moduleModifier : delta.getModuleModifiers()) {
                    if (moduleModifier instanceof ModifyClassModifier) {
                        //String methodID;
                        String prefix = ((ClassModifier) moduleModifier).qualifiedName();

                        for (Modifier mod : ((ModifyClassModifier) moduleModifier).getModifiers()) {
                            /*if (mod instanceof AddMethodModifier)
                                methodID = ((AddMethodModifier) mod).getMethodImpl().getMethodSig().getName();
                            else if (mod instanceof RemoveMethodModifier)
                                methodID = ((RemoveMethodModifier) mod).getMethodSig().getName();
                            else if (mod instanceof ModifyMethodModifier)
                                methodID = ((ModifyMethodModifier) mod).getMethodImpl().getMethodSig().getName();
                            else
                                continue;*/

                            if(mod instanceof DeltaTraitModifier) {
                                HashSet<String> methodIDSet = new HashSet<>();
                                ((DeltaTraitModifier) mod).collectMethodIDs(methodIDSet, model);

                                for (String methodID : methodIDSet) {
                                    result = result & doThings(pl, l, methodID, prefix, deltaID, cache);
                                }
                            }
                        }
                    } else if (moduleModifier instanceof AddClassModifier
                            || moduleModifier instanceof RemoveClassModifier) {

                        String prefix = ((ClassModifier) moduleModifier).qualifiedName();
                        if (cache.containsKey(prefix)) {
                            result = false;
                            assert ! cache.get(prefix).isEmpty();
                            String otherDeltaID = cache.get(prefix).values().iterator().next();
                            l.add(new TypeError(pl, ErrorMessage.AMBIGUOUS_PRODUCTLINE, pl.getName(), deltaID, otherDeltaID, prefix));
                        } else {
                            cache.put(prefix, new HashMap<String, String>());
                            cache.get(prefix).put("CLASS", deltaID);
                        }
                    }
                }
                // TODO apply same reasoning to other elements: fields,
                // functions, ADTs, etc
            }
        }
        // FIXME remove boolean result unless needed
        return result;
    }

    public static boolean doThings(ProductLine pl, SemanticConditionList l, String methodID, String prefix, String deltaID, Map<String, Map<String, String>> cache) {
        boolean result = true;
        if (cache.containsKey(prefix)) {
            if (cache.get(prefix).containsKey(methodID)) {
                result = false;
                l.add(new TypeError(pl, ErrorMessage.AMBIGUOUS_PRODUCTLINE, pl.getName(), deltaID, cache.get(prefix).get(methodID), prefix + ", method " + methodID));
            } else if (cache.get(prefix).containsKey("CLASS")) {
                result = false;
                l.add(new TypeError(pl, ErrorMessage.AMBIGUOUS_PRODUCTLINE, pl.getName(), deltaID, cache.get(prefix).get("CLASS"), prefix));
            } else {
                cache.get(prefix).put(methodID, deltaID);
            }
        } else {
            cache.put(prefix, new HashMap<String, String>());
            cache.get(prefix).put(methodID, deltaID);
        }
        return result;
    }


}
