/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
import static org.easymock.EasyMock.*;

import abs.backend.java.lib.runtime.ABSObject;
import abs.backend.java.lib.net.msg.Msg;
import abs.backend.java.lib.net.msg.ObjectTargetMsg;
import abs.backend.java.lib.net.msg.COGTargetMsg;

public class DefaultRouterTest {
    private NetNode node1;
    private NetNode node2;
    private NetNode node3;
    private NetNode node4;

    private NetCOG cog1;
    private NetCOG cog2;

    private ABSObject obj1;
    private ABSObject obj2;

    private Router currentRouter;
    private Router otherRouter;

    @Before 
    public void setUp() {
	node1 = createMock(NetNode.class);
	node2 = createMock(NetNode.class);
	node3 = createMock(NetNode.class);
	node4 = createMock(NetNode.class);

	cog1 = createMock(NetCOG.class);
	cog2 = createMock(NetCOG.class);

	obj1 = createMock(ABSObject.class);
	obj2 = createMock(ABSObject.class);

	currentRouter = new DefaultRouter(node1);
	otherRouter = new DefaultRouter(node2);
    }

    @Test
    public void updateExchange() {	
	currentRouter.register(cog1);
	currentRouter.register(obj1);
	otherRouter.register(cog2);
	otherRouter.register(obj2);
	currentRouter.update(otherRouter);

	assertEquals("cog must be at one hop distance", 1, currentRouter.getRouteEntry(cog1).getHops());
	assertEquals("direction for cog must be other node", node2, currentRouter.getRouteEntry(cog1).getNextNode());
	assertEquals("object must be at one hop distance", 1, currentRouter.getRouteEntry(obj1).getHops());
	assertEquals("direction for cog must be at other node", node2, currentRouter.getRouteEntry(obj1).getNextNode());
    }

    @Test
    public void updateBetter() {
	otherRouter.register(obj1);
	currentRouter.replace(obj1, node3, 3);
	currentRouter.update(otherRouter);

	assertEquals("route to object must be 1 hop", 1, currentRouter.getRouteEntry(obj1).getHops());	
	assertEquals("route must be other node", node2, currentRouter.getRouteEntry(obj1).getNextNode());
    }

    @Test
    public void updateWorse() {
	otherRouter.replace(obj1, node2, 2);
	currentRouter.replace(obj1, node3, 5);
	currentRouter.update(otherRouter);

	assertEquals("route to object must be 1 hop", 1, currentRouter.getRouteEntry(obj1).getHops());
    }

    @Test
    public void updateWorseSameSource() {
	otherRouter.register(obj1);
	currentRouter.replace(obj1, node3, 3);
	currentRouter.update(otherRouter);
	otherRouter.replace(obj1, node4, 1);
	currentRouter.update(otherRouter);

	assertEquals("route to object must be 2 hops", 2, currentRouter.getRouteEntry(obj1).getHops());
    }

    @Test
    public void registerObject() {
	currentRouter.register(obj1);
	
	assertTrue("object must be among registered objects", currentRouter.getRegisteredObjects().contains(obj1));
	assertEquals("route to obj1 must be 0 hops", 0, currentRouter.getRouteEntry(obj1).getHops());
	assertEquals("route to obj1 must node1", node1, currentRouter.getRouteEntry(obj1).getNextNode());
    }

    @Test(expected=IllegalArgumentException.class)
    public void registerObjectAgain() {
	currentRouter.register(obj1);
	currentRouter.register(obj1);
    }
    
    @Test
    public void registerCOG() {
	currentRouter.register(cog1);
	assertTrue("object must be among registered objects", currentRouter.getRegisteredCOGs().contains(cog1));
	assertEquals("route to cog1 must be 0 hops", 0, currentRouter.getRouteEntry(cog1).getHops());
	assertEquals("route to cog1 must node1", node1, currentRouter.getRouteEntry(cog1).getNextNode());
    }

    @Test(expected=IllegalArgumentException.class)
    public void registerCOGAgain() {
	currentRouter.register(cog1);
	currentRouter.register(cog1);
    }

    @Test
    public void replaceExistingObject() {
	currentRouter.register(obj1);
	currentRouter.replace(obj1, node2, 1);
	assertEquals("route to cog1 must be 1 hops", 1, currentRouter.getRouteEntry(cog1).getHops());
	assertEquals("route to cog1 must node2", node2, currentRouter.getRouteEntry(cog1).getNextNode());
    }

    @Test 
    public void replaceNonexistentObject() {
	currentRouter.replace(obj1, node2, 1);
	assertEquals("route to obj1 must be 1 hops", 1, currentRouter.getRouteEntry(obj1).getHops());
	assertEquals("route to obj1 must node2", node2, currentRouter.getRouteEntry(obj1).getNextNode());
    }

    @Test
    public void replaceExistingCOG() {
	currentRouter.register(cog1);
	currentRouter.replace(cog1, node2, 1);
	assertEquals("route to cog1 must be 1 hops", 1, currentRouter.getRouteEntry(cog1).getHops());
	assertEquals("route to cog1 must node2", node2, currentRouter.getRouteEntry(cog1).getNextNode());
    }

    @Test 
    public void replaceNonexistentCOG() {
	currentRouter.replace(cog1, node2, 1);
	assertEquals("route to cog1 must be 1 hops", 1, currentRouter.getRouteEntry(cog1).getHops());
	assertEquals("route to cog1 must node2", node2, currentRouter.getRouteEntry(cog1).getNextNode());
    }

    @Test
    public void noRegisteredObjects() {
	assertTrue("there exists registered objects", currentRouter.getRegisteredObjects().isEmpty());
    }
    
    @Test
    public void noRegisteredCOGs() {
	assertTrue("there exists registered cogs", currentRouter.getRegisteredCOGs().isEmpty());
    }

    @Test
    public void nextNodeIsSelf() {
	ObjectTargetMsg msg = createMock(ObjectTargetMsg.class);	
	currentRouter.register(obj1);
	expect(msg.getTarget()).andReturn(obj1);
	replay(msg);
	
	assertEquals("next node must be node1", node1, currentRouter.getNextNode(msg));	
	verify(msg);
    }

    @Test
    public void nextNodeIsSelectedNeighbour() {
	ObjectTargetMsg msg = createMock(ObjectTargetMsg.class);
	currentRouter.replace(obj1, node2, 1);
	expect(msg.getTarget()).andReturn(obj1);
	replay(msg);

	assertEquals("next node must be node2", node2, currentRouter.getNextNode(msg));
	verify(msg);
    }

    @Test
    public void nextNodeIsDefaultNeighbour() {
	Msg msg = createMock(Msg.class);
	assertEquals("next node must be node2", node2, currentRouter.getNextNode(msg));
    }

    @Test
    public void nextNodeBasedOnCOG() {
	COGTargetMsg msg = createMock(COGTargetMsg.class);
	currentRouter.replace(cog1, node2, 1);
	expect(msg.getTarget()).andReturn(cog1);
	replay(msg);

	assertEquals("next node must be node2", node2, currentRouter.getNextNode(msg));
	verify(msg);
    }

}
