/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;


/* Sorting object that computes a list of elements sorted according to a given partial order
 * Usage:
 *     1. instantiate, giving all elements to be sorted
 *     2. define partial order by repeatedly calling addEdge(e1, e2), where e1 > e2
 *     3. obtain a valid order with getPreferredOrder() or getAnOrder()
 */
public class TopologicalSorting<T> {

    private final Map<T, Set<T>> graph; // maps N to all nodes that N depends on
    private List<Set<T>> partition; // null if sorting is necessary
    private List<T> preferredOrder; // ditto

    public TopologicalSorting(Set<T> nodes) {
        if (nodes.size() == 0) {
            partition = Collections.emptyList();
            preferredOrder = Collections.emptyList();
            graph = Collections.emptyMap();
        } else {
            partition = null;
            preferredOrder = null;
            graph = new HashMap<>();
            nodes.forEach(node -> graph.put(node, new HashSet<T>()));
        }
    }

    public void addEdge(T high, T low) throws DeltaModellingException {
        if (graph.isEmpty())
            throw new DeltaModellingException("Sorting: cannot add edge, [" + high.toString() + "; " + low.toString() + "] -- graph is empty");
        if (!graph.containsKey(high))
            throw new DeltaModellingException("Sorting: cannot add edge, node [" + high.toString() + "] not in graph");
        if (!graph.containsKey(low))
            throw new DeltaModellingException("Sorting: cannot add edge, node [" + low.toString() + "] not in graph");
        graph.get(low).add(high); // `low` points to all nodes `high` it depends on
        partition = null;       // reset sorting status
        preferredOrder = null;
    }

    private void sort() throws DeltaModellingException {

        if (partition != null) return;
        else partition = new ArrayList<>();

        Set<T> nodes = new HashSet<>(graph.keySet());

        while (nodes.size() > 0) {
            HashSet<T> currentSet = new HashSet<>();
            partition.addLast(currentSet);

            for (T node : nodes) {
                boolean rootNode = true;
                for (T cn : nodes) {
                    // Note this checks for self-dependency since we
                    // don't skip `node` itself when checking `cn->node`
                    if (graph.get(node).contains(cn)) {
                        rootNode = false;
                        break;  // not a root node
                    }
                }
                if (rootNode)
                    currentSet.add(node);
            }
            // no nodes in set means there is a cycle among the remaining nodes
            if (currentSet.isEmpty())
                throw new DeltaModellingException("Sorting: cycle detected among the following nodes: " + nodes.toString());

            // Remove newly-found root nodes
            for (T node : currentSet)
                nodes.remove(node);
        }
    }

    /**
     * Get a single, valid order
     *
     * <p>TODO: eventually this should compute an
     * implication-determined order (cf. Damiani and Schaefer 2012),
     * which yields a PFGT with a minimal number of nodes
     *
     * @return A (possibly empty) list of elements
     */
    public List<T> getPreferredOrder() {
        if (preferredOrder != null) // only compute once
            return preferredOrder;

        ensureSorted();
        preferredOrder = new ArrayList<>(graph.size());
        for (Set<T> set : partition)
            preferredOrder.addAll(set);

        return preferredOrder;
    }

    /**
     * Returns a single, valid order
     *
     * @return A (possibly empty) list of elements
     */
    public List<T> getAnOrder() {
        return getPreferredOrder();
    }

    /**
     * The delta partition is an ordered list of sets of deltas.  All
     * deltas in a certain set have the same precedence, that is, they
     * can be applied in any order.
     *
     * @return A (possibly empty) list of non-empty sets.
     */
    public List<Set<T>> getPartition() {
        ensureSorted();
        return partition;
    }


    /*
     * Make sure we called sort() before we access the results
     */
    private void ensureSorted() {
        if (partition == null)
            sort();
    }
}
