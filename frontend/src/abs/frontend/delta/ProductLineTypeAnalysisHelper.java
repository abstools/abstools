/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

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

        // Check strong unambiguity
        checkStrongUnambiguity(pl, errors);

        // Build the product family generation trie. Hereby check that
        // - product generation mapping is total
        // - TODO all products are well-typed programs
        DeltaTrie pfgt = buildPFGT(pl, errors);

//        System.out.println(pfgt);

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

//        for (Set<String> set : pl.getDeltaPartition()) {
//            System.out.print("Delta partition: {");
//            for (String el : set)
//                System.err.print(" " + el + " ");
//            System.out.print("}   ");
//        }
//        System.out.println();

        for (Set<String> set : pl.getDeltaPartition()) {

            // Remember the names of classes and methods modified by deltas in
            // current set
            // { Module.Class -> { Method -> Delta } }
            // { Module.Class -> { "CLASS" -> Delta } }
            Map<String, Map<String, String>> cache = new HashMap<String, Map<String, String>>();

            for (String deltaID : set) {
                DeltaDecl delta = model.getDeltaDeclsMap().get(deltaID); // assumes the DeltaDecl exists!

                for (ModuleModifier moduleModifier : delta.getModuleModifiers()) {
                    if (moduleModifier instanceof ModifyClassModifier) {
                        String methodID;
                        String prefix = ((ClassModifier) moduleModifier).qualifiedName();

                        for (Modifier mod : ((ModifyClassModifier) moduleModifier).getModifiers()) {
                            if (mod instanceof AddMethodModifier)
                                methodID = ((AddMethodModifier) mod).getMethodImpl().getMethodSig().getName();
                            else if (mod instanceof RemoveMethodModifier)
                                methodID = ((RemoveMethodModifier) mod).getMethodSig().getName();
                            else if (mod instanceof ModifyMethodModifier)
                                methodID = ((ModifyMethodModifier) mod).getMethodImpl().getMethodSig().getName();
                            else
                                continue;

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
                        }
                    } else if (moduleModifier instanceof AddClassModifier
                            || moduleModifier instanceof RemoveClassModifier) {

                        String prefix = ((ClassModifier) moduleModifier).qualifiedName();
                        if (cache.containsKey(prefix)) {
                            result = false;
                            l.add(new TypeError(pl, ErrorMessage.AMBIGUOUS_PRODUCTLINE, pl.getName(), deltaID, cache.get(prefix).get("CLASS"), prefix));
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
        return result;
    }

    public static void checkStrongUnambiguity(ProductLine pl, SemanticConditionList l) {
        isStronglyUnambiguous(pl, l);
    }

}
