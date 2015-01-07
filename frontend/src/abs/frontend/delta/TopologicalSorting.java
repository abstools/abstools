/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.ArrayTable;
import com.google.common.collect.Table;
import com.google.common.collect.Collections2;
import com.google.common.math.BigIntegerMath;

public class TopologicalSorting<T> {

    private final Set<T> nodes;
    private final Table<T, T, Boolean> incidence;

    List<Set<T>> partition;

    private List<T> preferredOrder;
    private ListComparator preferredOrderComparator;
    private Set<List<T>> allOrders;

    public TopologicalSorting(Set<T> nodes) {
        this.nodes = nodes;

        if (nodes.size() == 0) {
            preferredOrder = Collections.emptyList();
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

    /*
     * A Comparator backed by the preferredOrder
     * - Makes it more efficient to sort a given list of Ts based on the preferredOrder
     */
    private class ListComparator implements Comparator<T> {
        private Map<T, Integer> map;

        public ListComparator(final List<T> list) {
            map = new HashMap<T, Integer>(list.size());
            for (int i=0; i < list.size(); i++)
                map.put(list.get(i), i);
        }

        @Override
        public int compare(T o1, T o2) {
            return map.get(o1) - map.get(o2);
        }
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
     *
     * TODO: eventually this should compute an implication-determined order, which yields a PFGT with a minimal number of nodes
     */
    public List<T> getPreferredOrder() {
        if (preferredOrder != null) // only compute once
            return preferredOrder;

        preferredOrder = new ArrayList<T>(nodes.size());
        for (Set<T> set : partition)
            preferredOrder.addAll(set);

        preferredOrderComparator = new ListComparator(preferredOrder);

        return preferredOrder;
    }

    public List<T> getAnOrder() {
        return getPreferredOrder();
    }

    /*
     * Sort given list of Ts according to the preferredOrder
     */
    public List<T> sortList(List<T> list) {
        Collections.sort(list, preferredOrderComparator);
        return list;
    }

    /*
     * Sort given set of Ts according to the preferredOrder
     */
    public List<T> sortSet(Set<T> set) {
        List<T> list = new ArrayList<T>(set);
        return sortList(list);
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
