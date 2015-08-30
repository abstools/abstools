/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.Arrays;
import java.util.HashSet;

import org.junit.Test;

import abs.frontend.ast.DeltaID;

public class TopologicalSortingTest extends DeltaTest {
    int numNodes;
    DeltaID[] deltas;

    @Test(expected=DeltaModellingException.class)
    public void test0_1() {
        TopologicalSorting<DeltaID> sorter = init0_1();
        sorter.sort();
    }

    @Test(expected=DeltaModellingException.class)
    public void testCycle() {
        numNodes = 4;
        deltas = new DeltaID[numNodes];
        for (int i=0; i<numNodes; i++)
            deltas[i] = new DeltaID("D" + i);
        TopologicalSorting<DeltaID> sorter = new TopologicalSorting<DeltaID>(new HashSet<DeltaID>(Arrays.asList(deltas)));
        sorter.addEdge(deltas[0], deltas[1]); // cycle
        sorter.addEdge(deltas[1], deltas[0]); // cycle
        sorter.addEdge(deltas[2], deltas[0]);
        sorter.addEdge(deltas[1], deltas[3]);
        sorter.sort();
    }

    @Test(expected=DeltaModellingException.class)
    public void testCycle2() {
        numNodes = 1;
        deltas = new DeltaID[numNodes];
        deltas[0] = new DeltaID("D0");
        TopologicalSorting<DeltaID> sorter = new TopologicalSorting<DeltaID>(new HashSet<DeltaID>(Arrays.asList(deltas)));
        sorter.addEdge(deltas[0], deltas[0]); // cycle
        sorter.sort();
    }

    /**************************************************************************/

    private TopologicalSorting<DeltaID> init0_1() {
        numNodes = 0;
        deltas = new DeltaID[numNodes];
        TopologicalSorting<DeltaID> sorter = new TopologicalSorting<DeltaID>(new HashSet<DeltaID>(Arrays.asList(deltas)));
        sorter.addEdge(new DeltaID("D0"), new DeltaID("D1"));
        return sorter;
    }

}
