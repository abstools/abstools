/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
import static org.easymock.EasyMock.*;

public class DefaultRouteEntryTest {
    private NetNode node1;
    private NetNode node2;
    private DefaultRouteEntry entry;

    @Before
    public void setUp() {
	node1 = createMock(NetNode.class);
	node2 = createMock(NetNode.class);
	entry = new DefaultRouteEntry(node1, 2, node2);
    }

    @Test(expected=IllegalArgumentException.class)
    public void negativeHops() {
	RouteEntry other = new DefaultRouteEntry(node1, -1, node1);
    }

    @Test
    public void implicitZeroHops() {
	RouteEntry other = new DefaultRouteEntry(node1, node1);
	assertEquals("must be equal to zero hops", 0, other.getHops());
    }

    @Test
    public void getNextNode() {
	assertEquals("must be equal to node", node1, entry.getNextNode());
    }

    @Test
    public void getHops() {
	assertEquals("must be equal to hops", 2, entry.getHops());
    }

    @Test
    public void getSourceNode() {

    }

    @Test
    public void equalsSelf() {
	assertEquals("must be equal to self", entry, entry);
    }

    @Test
    public void doesEqual() {
	RouteEntry other = new DefaultRouteEntry(entry.getNextNode(), entry.getHops(), entry.getSourceNode());
	assertEquals("must be equal to entry", entry, other);

    }

    @Test
    public void doesNotEqualNode() {
	RouteEntry other = new DefaultRouteEntry(node2, entry.getHops(), entry.getSourceNode());
	assertFalse("must not be equal to entry", other.equals(entry));
    }

    @Test
    public void doesNotEqualHops() {
	RouteEntry other = new DefaultRouteEntry(entry.getNextNode(), entry.getHops() + 1, entry.getSourceNode());
	assertFalse("must not be equal to entry", other.equals(entry));
    }

    @Test
    public void doesNotEqualSource() {
	RouteEntry other = new DefaultRouteEntry(entry.getNextNode(), entry.getHops(), node1);
	assertFalse("must not be equal to entry", other.equals(entry));
    }

    @Test
    public void comparesToLess() {
	RouteEntry other = new DefaultRouteEntry(node2, entry.getHops() + 1, entry.getSourceNode());
	assertTrue("must be less than other", entry.compareTo(other) < 0);
	assertTrue("must be greater than entry", other.compareTo(entry) > 0);
    }

    @Test
    public void comparesToGreater() {
	RouteEntry other = new DefaultRouteEntry(node2, entry.getHops() + 1, entry.getSourceNode());
	assertTrue("must be less than other", other.compareTo(entry) > 0);
	assertTrue("must be greater than entry", entry.compareTo(other) < 0);
    }

    @Test
    public void comparesToEqual() {
	RouteEntry other = new DefaultRouteEntry(node2, entry.getHops(), entry.getSourceNode());
	assertEquals("must be equal", 0, other.compareTo(entry));
	assertEquals("must be equal", 0, entry.compareTo(other));
    }

    @Test 
    public void comparesToSelf() {
	assertEquals("must be equal", 0, entry.compareTo(entry));
    }

}
