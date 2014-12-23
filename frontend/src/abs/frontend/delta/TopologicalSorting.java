/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.common.collect.ArrayTable;
import com.google.common.collect.Table;
import com.google.common.collect.Collections2;
import com.google.common.math.BigIntegerMath;

public class TopologicalSorting<T> {

    private final Set<T> nodes;
    private final Table<T, T, Boolean> incidence;

    List<Set<T>> partition;
    
    private List<T> anOrder;
    private Set<List<T>> allOrders;

    public TopologicalSorting(Set<T> nodes) {
        this.nodes = nodes;

        if (nodes.size() == 0) {
            anOrder = Collections.emptyList();
            allOrders = Collections.emptySet();
            incidence = null;
            return;
        }
        
        incidence = ArrayTable.create(nodes, nodes);
        for (T cn : nodes)
            for (T rn : nodes)
                incidence.put(cn, rn, false);
        
        partition = new ArrayList<Set<T>>();

    }
    
    public List<Set<T>> getPartition() {
        return partition;
    }
    
    public void addEdge(T high, T low) throws DeltaModellingException {
        if (incidence == null)
            throw new DeltaModellingException("Nodes not found [" + high.toString() + "; " + low.toString() + "] -- graph is empty");
        if (! incidence.containsColumn(high))
            throw new DeltaModellingException("Node not found [" + high.toString() + "]");
        if (! incidence.containsRow(low))
            throw new DeltaModellingException("Node not found [" + low.toString() + "]");
        incidence.put(high, low, true);
    }

    public void sort() throws DeltaModellingException {
        boolean rootNode;
        Set<T> nodes = new HashSet<T>(this.nodes);

        int currentSet = 0;
        while (nodes.size() > 0) {
            partition.add(new HashSet<T>());

            for (T node : nodes) {
                rootNode = true;
                for (T cn : nodes) {
                    if (incidence.get(cn, node) == true) {
                        rootNode = false;
                        break; // not a root node
                    }
                }
                if (rootNode)
                    partition.get(currentSet).add(node);
            }
            // no nodes in set means there is a cycle among the remaining nodes
            if (partition.get(currentSet).isEmpty())
                throw new DeltaModellingException("Cycle detected among the following nodes: " + nodes.toString());
            
            // for all nodes in set: remove these nodes from node set
            for (T node : partition.get(currentSet))
                nodes.remove(node);
            
            currentSet++;
        }
    }
    
    /*
     * Returns a single, valid delta order
     */
    public List<T> getAnOrder() {
        if (anOrder != null) // only compute once
            return anOrder;
        
        anOrder = new ArrayList<T>(nodes.size());
        for (Set<T> set : partition)
            anOrder.addAll(set);
        return anOrder;
    }
    
    /*
     * Returns all valid delta orders (Product family generation strings - PFGS)
     * The number of orders can get very large very fast: with n independent deltas there are n! possible orders
     * 
     */
    public Set<List<T>> getAllOrders() {
        if (allOrders != null)  // only compute once
            return allOrders;
        
        allOrders = new HashSet<List<T>>();
        List<Collection<List<T>>> grouplist = new ArrayList<Collection<List<T>>>(partition.size());

        for (Set<T> set : partition) {
            // Add all permutations in this group
            // There are n! permutations in a group of size n
            // TODO Issue a warning when group larger than, say, 8? Here or when type checking.
            //System.out.println("Group (size " + BigIntegerMath.factorial(set.size()) + ")");
            grouplist.add(Collections2.permutations(set));
        }
        
        generateCombinations(grouplist, allOrders, 0, new ArrayList<T>());
        return allOrders;
    }
    
    private void generateCombinations(List<Collection<List<T>>> grouplist, Set<List<T>> results, int depth, List<T> current) {
        if(depth == grouplist.size()) {
            results.add(new ArrayList<T>(current));
            return;
        }
        for (List<T> permutation : grouplist.get(depth)) {
            List<T> current2 = new ArrayList<T>(current);
            current2.addAll(permutation);
            generateCombinations(grouplist, results, depth + 1, current2);
        }
    }
}
