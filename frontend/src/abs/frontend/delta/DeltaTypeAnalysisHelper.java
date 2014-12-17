/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.List;

import abs.common.StringUtils;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.*;

public class DeltaTypeAnalysisHelper {

    /* 
     * Ordered partition of the set of delta modules 
     * 
     */
    public static List<Set<String>> getDeltaPartitions(ProductLine pl, SemanticErrorList l) {

        TopologicalSorting<String> sorter = new TopologicalSorting<String>(getAllDeltas(pl));

        for (DeltaClause clause : pl.getDeltaClauses()) {
            String deltaid = clause.getDeltaspec().getName();
            for (DeltaID did : clause.getAfterDeltaIDs()) {
                String afterid = did.getName();
                try {
                    sorter.addEdge(afterid, deltaid);
                } catch (DeltaModellingException e) {
                    // a node is not part of the graph, meaning that the corresponding delta has no delta clause
                    l.add(new TypeError(pl, ErrorMessage.ERROR_IN_PRODUCT_LINE_UNUSED_DELTA, pl.getName())); 
                }
            }
        }
        try {
            sorter.sort();
        } catch (DeltaModellingException e) {
            // cycle in graph -> no order exists
            l.add(new TypeError(pl, ErrorMessage.ERROR_IN_PRODUCT_LINE_DELTA_ORDER, pl.getName())); 
        }

        return sorter.getPartitions();
    }
    
    public static void getMinimalTrie() {
        
    }
    
    /*
     * Return set of all deltas for which a delta clause exists in the productline declaration
     */
    public static Set<String> getAllDeltas(ProductLine pl) {
        Set<String> allDeltas = new HashSet<String>(pl.getDeltaClauses().getNumChild());
        for (DeltaClause clause : pl.getDeltaClauses())
            allDeltas.add(clause.getDeltaspec().getName());
        return allDeltas;
    }
    
    /*
     * An IFΔJ product line is type-safe if the following conditions hold: (i) 
     * it is strongly unambiguous, (ii) its product generation mapping is total, 
     * and (iii) all its products are well-typed IFJ programs.
     * 
     */
    public static boolean isTypeSafe(ProductLine pl, List<Set<String>> deltaPartitions, SemanticErrorList l) {
        return isStronglyUnambiguous(pl, deltaPartitions, l);
    }

    /*
     * For strong unambiguity, two delta modules that modify/add/remove the same method cannot be 
     * placed in the same partition even if they are never applied together. 
     * 
     * We iterate over each delta partition and check whether any two deltas in the same partition
     * modify/add/remove the same method.
     */
    public static boolean isStronglyUnambiguous(ProductLine pl, List<Set<String>> deltaPartitions, SemanticErrorList l) {
        
        // TODO make sure all deltas mentioned in DeltaClauses have a corresponding DeltaDecl
        
        boolean result = true;
        Model model = pl.getModel();
        
        for (Set<String> part : deltaPartitions) {
            String methodID;
            for (String deltaID : part) {
                // Cache the qualified names of all methods modified in this partition
                Map<String, String> cache = new HashMap<String, String>();

                DeltaDecl delta = model.getDeltaDeclsMap().get(deltaID);
                
                for (ModuleModifier moduleModifier : delta.getModuleModifiers()) {
                    if (moduleModifier instanceof ModifyClassModifier) {
                        String prefix = ((ModifyClassModifier) moduleModifier).findClass().qualifiedName();
                        
                        for (Modifier mod : ((ModifyClassModifier) moduleModifier).getModifiers() ) {
                            /* TODO We currently only care about methods - 
                             * but the same reasoning applies to other elements: fields, functions, ADTs, etc
                             */ 
                            if (mod instanceof AddMethodModifier)
                                methodID = ((AddMethodModifier) mod).getMethodImpl().getMethodSig().getName();
                            else if (mod instanceof RemoveMethodModifier)
                                methodID = ((RemoveMethodModifier) mod).getMethodSig().getName(); 
                            else if (mod instanceof ModifyMethodModifier)
                                methodID = ((ModifyMethodModifier) mod).getMethodImpl().getMethodSig().getName(); 
                            else
                                continue;

                            // Fully qualify methodID (Module.Class.method)
                            methodID = StringUtils.joinNames(prefix, methodID);
                            if (cache.containsKey(methodID)) {
                                l.add(new TypeError(pl, ErrorMessage.AMBIGUOUS_PRODUCTLINE, deltaID, cache.get(methodID), methodID));
                                result = false;
                            } else {
                                cache.put(methodID, deltaID);
                            }
                        }
                    
                    }
                        
                }
            }
        }

        for (String deltaID : model.getDeltaDeclsMap().keySet()) {
            
        }
        
        for (String deltaID : model.getDeltaDeclsMap().keySet()) {
            //DeltaDecl delta = 
        }
        
        for (DeltaDecl delta : model.getDeltaDeclsMap().values()) {
            
        }
        
        return result;
    }
}
