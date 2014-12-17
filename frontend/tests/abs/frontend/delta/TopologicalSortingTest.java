/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.Test;

import abs.frontend.ast.DeltaID;

public class TopologicalSortingTest extends DeltaTest {
    int numNodes;
    DeltaID[] deltas;
    
    @Test
    public void test0_0() {
        TopologicalSorting<DeltaID> sorter = init0_0();
        sorter.sort();
        List<DeltaID> order = sorter.getAnOrder();
        assertEquals(order, Collections.emptyList());
        Set<List<DeltaID>> orders = sorter.getAllOrders();
        assertEquals(orders, Collections.emptySet());
    }

    @Test(expected=DeltaModellingException.class)
    public void test0_1() {
        TopologicalSorting<DeltaID> sorter = init0_1();
        sorter.sort();
    }
    
    @Test
    public void test1_0() {
        TopologicalSorting<DeltaID> sorter = init1_0();
        sorter.sort();
        Set<List<DeltaID>> orders = sorter.getAllOrders();
        assertEquals(orders.size(), 1);
    }

    @Test
    public void test3_0() {
        TopologicalSorting<DeltaID> sorter = init3_0();
        sorter.sort();
        Set<List<DeltaID>> orders = sorter.getAllOrders();
        assertEquals(orders.size(), 6);
    }
    
    @Test
    public void test3_2() {
        TopologicalSorting<DeltaID> sorter = init3_2();
        sorter.sort();
        Set<List<DeltaID>> orders = sorter.getAllOrders();
        assertEquals(orders.size(), 2);
    }
    
    @Test
    public void test8_5() {
        TopologicalSorting<DeltaID> sorter = init8_5();
        sorter.sort();
        
        assertTrue(precedenceCheck(sorter.getAnOrder(), deltas[0], deltas[1]));
        assertTrue(precedenceCheck(sorter.getAnOrder(), deltas[0], deltas[2]));
        assertTrue(precedenceCheck(sorter.getAnOrder(), deltas[3], deltas[4]));
        assertTrue(precedenceCheck(sorter.getAnOrder(), deltas[4], deltas[5]));
        assertTrue(precedenceCheck(sorter.getAnOrder(), deltas[6], deltas[7]));

        Set<List<DeltaID>> orders = sorter.getAllOrders();
        assertEquals(orders.size(), 6*6*2);
        
        for (List<DeltaID> order : orders) {
            assertTrue(precedenceCheck(order, deltas[0], deltas[1]));
            assertTrue(precedenceCheck(order, deltas[0], deltas[2]));
            assertTrue(precedenceCheck(order, deltas[3], deltas[4]));
            assertTrue(precedenceCheck(order, deltas[4], deltas[5]));
            assertTrue(precedenceCheck(order, deltas[6], deltas[7]));
        }
    }
    
    @Test
    public void test10_0() {
        TopologicalSorting<DeltaID> sorter = init10_0();
        sorter.sort();
//        long startTime = System.currentTimeMillis();
        Set<List<DeltaID>> orders = sorter.getAllOrders(); // this takes 0.5s on my machine
//        long stopTime = System.currentTimeMillis();
//        System.out.println("time: " + (stopTime - startTime));
        assertEquals(orders.size(), 362880 /*9!*/);
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

    private TopologicalSorting<DeltaID> init0_0() {
        numNodes = 0;
        deltas = new DeltaID[numNodes];
        TopologicalSorting<DeltaID> sorter = new TopologicalSorting<DeltaID>(new HashSet<DeltaID>(Arrays.asList(deltas)));
        return sorter;
    }

    private TopologicalSorting<DeltaID> init0_1() {
        numNodes = 0;
        deltas = new DeltaID[numNodes];
        TopologicalSorting<DeltaID> sorter = new TopologicalSorting<DeltaID>(new HashSet<DeltaID>(Arrays.asList(deltas)));
        sorter.addEdge(new DeltaID("D0"), new DeltaID("D1"));
        return sorter;
    }
    
    private TopologicalSorting<DeltaID> init1_0() {
        numNodes = 1;
        deltas = new DeltaID[numNodes];
        deltas[0] = new DeltaID("D0");
        TopologicalSorting<DeltaID> sorter = new TopologicalSorting<DeltaID>(new HashSet<DeltaID>(Arrays.asList(deltas)));
        return sorter;
    }
    
    private TopologicalSorting<DeltaID> init3_0() {
        numNodes = 3;
        deltas = new DeltaID[numNodes];
        for (int i=0; i<numNodes; i++)
            deltas[i] = new DeltaID("D" + i);

        // no edges
        TopologicalSorting<DeltaID> sorter = new TopologicalSorting<DeltaID>(new HashSet<DeltaID>(Arrays.asList(deltas)));
        return sorter;
    }

    private TopologicalSorting<DeltaID> init3_2() {
        numNodes = 3;
        deltas = new DeltaID[numNodes];
        for (int i=0; i<numNodes; i++)
            deltas[i] = new DeltaID("D" + i);
        
        // no edges
        TopologicalSorting<DeltaID> sorter = new TopologicalSorting<DeltaID>(new HashSet<DeltaID>(Arrays.asList(deltas)));
        sorter.addEdge(deltas[2], deltas[0]);
        sorter.addEdge(deltas[2], deltas[1]);
        return sorter;
    }

    private TopologicalSorting<DeltaID> init8_5() {
        numNodes = 8;
        deltas = new DeltaID[numNodes];
        for (int i=0; i<numNodes; i++)
            deltas[i] = new DeltaID("D" + i);
        
        TopologicalSorting<DeltaID> sorter = new TopologicalSorting<DeltaID>(new HashSet<DeltaID>(Arrays.asList(deltas)));
        sorter.addEdge(deltas[0], deltas[1]);
        sorter.addEdge(deltas[1], deltas[2]);
        sorter.addEdge(deltas[3], deltas[4]);
        sorter.addEdge(deltas[4], deltas[5]);
        sorter.addEdge(deltas[6], deltas[7]);
        return sorter;
    }
    
    private TopologicalSorting<DeltaID> init10_0() {
        numNodes = 9;
        deltas = new DeltaID[numNodes];
        for (int i=0; i<numNodes; i++)
            deltas[i] = new DeltaID("D" + i);
        
        // no edges
        TopologicalSorting<DeltaID> sorter = new TopologicalSorting<DeltaID>(new HashSet<DeltaID>(Arrays.asList(deltas)));
        return sorter;
    }

    private boolean precedenceCheck(List<DeltaID> d, DeltaID a, DeltaID b) {
        for (int i=0; i<numNodes; i++) {
            if (d.get(i) == a)
                for (int j=i+1; j < numNodes; j++) {
                    if (d.get(j) == b)
                        return true;
                }
        }
        return false;
    }

}
