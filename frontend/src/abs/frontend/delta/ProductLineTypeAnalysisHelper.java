/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.*;

public class ProductLineTypeAnalysisHelper {

    /*
     * An IFΔJ product line is type-safe if the following conditions hold: (i)
     * it is strongly unambiguous, (ii) its product generation mapping is total,
     * and (iii) all its products are well-typed IFJ programs.
     */
    public static void typeCheckPL(ProductLine pl, SemanticErrorList errors) {

        TopologicalSorting<String> sorter = new TopologicalSorting<String>(pl.getAllDeltaIDs());

        // Check strong unambiguity
        List<Set<String>> deltaPartition = getDeltaPartition(pl, sorter, errors);
        checkStrongUnambiguity(pl, deltaPartition, errors);

        //TODO check the other conditions:
        // - product generation mapping is total
        // - all products are well-typed programs

        // Build the product family generation trie, including:
        // - a ProgramTypeAbstraction for each node
        // - check well-typedness of ProgramTypeAbstraction for each node that corresponds to a valid product
        DeltaTrie pfgt = buildPFGT(pl, sorter, errors);

        System.out.println(pfgt);

    }


    public static DeltaTrie buildPFGT(ProductLine pl, TopologicalSorting<String> sorter, SemanticErrorList errors) {
        Model model = pl.getModel();

        // build PFG Trie
        DeltaTrie trie = new DeltaTrie(errors);
        trie.setPl(pl);

        for (ImplicitProduct product : model.getImplicitProductList()) {
            // for each product: obtain sequence of deltas
            Set<String> deltas = pl.findApplicableDeltas(product);
            // sort deltas
            List<String> productGenerationString = sorter.sortSet(deltas);
            trie.addWord(productGenerationString);
        }
        return trie;
    }

    /*
     * Ordered partition of the set of delta modules
     */
    public static List<Set<String>> getDeltaPartition(ProductLine pl, TopologicalSorting<String> sorter, SemanticErrorList l) {


        for (DeltaClause clause : pl.getDeltaClauses()) {
            String deltaid = clause.getDeltaspec().getDeltaID();
            for (DeltaID did : clause.getAfterDeltaIDs()) {
                String afterid = did.getName();
                try {
                    sorter.addEdge(afterid, deltaid);
                } catch (DeltaModellingException e) {
                    // a node is not part of the graph, meaning that the
                    // corresponding delta has no delta clause
                    l.add(new TypeError(pl, ErrorMessage.ERROR_IN_PRODUCT_LINE_UNUSED_DELTA, pl.getName(), did.getName()));
                    return Collections.<Set<String>> emptyList();
                }
            }
        }
        try {
            sorter.sort();
        } catch (DeltaModellingException e) {
            // cycle in graph -> no order exists
            l.add(new TypeError(pl, ErrorMessage.ERROR_IN_PRODUCT_LINE_DELTA_ORDER, pl.getName()));
            return Collections.<Set<String>> emptyList();
        }

        return sorter.getPartition();
    }


    /*
     * A product line is strongly unambiguous if each set in the partition of
     * the delta modules specified in the product-line declaration is
     * consistent, that is, if one delta module in a set adds or removes a
     * class, no other delta module in the same set may add, remove or modify
     * the same class, and the modifications of the same class in different
     * delta modules in the same set have to be disjoint.
     */
    public static boolean isStronglyUnambiguous(ProductLine pl, List<Set<String>> deltaPartition, SemanticErrorList l) {

        boolean result = true;
        Model model = pl.getModel();

        for (Set<String> set : deltaPartition) {

            // Remember the names of classes and methods modified by deltas in
            // current set
            // { Module.Class -> { Method -> Delta } }
            // { Module.Class -> { "CLASS" -> Delta } }
            Map<String, Map<String, String>> cache = new HashMap<String, Map<String, String>>();

            for (String deltaID : set) {
                DeltaDecl delta = model.getDeltaDeclsMap().get(deltaID);

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

    public static void checkStrongUnambiguity(ProductLine pl, List<Set<String>> deltaPartition, SemanticErrorList l) {
        isStronglyUnambiguous(pl, deltaPartition, l);
    }

}
