/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import abs.backend.java.lib.runtime.ABSObject;
import abs.backend.java.lib.runtime.COG;

public class DefaultRouterTest {

    private NetNode node1;
    private NetNode node2;
    private NetNode node3;

    private COG cog1;
    private COG cog2;

    private ABSObject obj1;
    private ABSObject obj2;

    @Before 
    public void setUp() {
	node1 = new NodeImpl(0);
	node2 = new NodeImpl(1);
	node3 = new NodeImpl(2);
	cog1 = null;
	cog2 = null;
	obj1 = null;
	obj2 = null;
    }

    @Test
    public void updateExchange() {
	Router currentRouter = new DefaultRouter(node1);
	Router otherRouter = new DefaultRouter(node2);

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
	Router currentRouter = new DefaultRouter(node1);
	Router otherRouter = new DefaultRouter(node2);

	otherRouter.register(obj1);
	currentRouter.replace(obj1, node3, 3);

	currentRouter.update(otherRouter);

	assertEquals("route to object must be 1 hop", 1, currentRouter.getRouteEntry(obj1).getHops());	
	assertEquals("route must be other node", node2, currentRouter.getRouteEntry(obj1).getNextNode());
    }

    @Test
    public void updateWorse() {
	Router currentRouter = new DefaultRouter(node1);
	Router otherRouter = new DefaultRouter(node2);

	otherRouter.replace(obj1, node2, 2);
	currentRouter.replace(obj1, node3, 5);

	currentRouter.update(otherRouter);

	assertEquals("route to object must be 1 hop", 1, currentRouter.getRouteEntry(obj1).getHops());
    }

    @Test
    public void registerObject() {
	Router router = new DefaultRouter(node1);
	router.register(obj1);
	assertTrue("object must be among registered objects", router.getRegisteredObjects().contains(new DefaultRouteEntry(node1, 0)));
    }
    
    @Test
    public void registerCOG() {
	
    }

    @Test
    public void replaceExistingObject() {

    }

    @Test 
    public void replaceNonexistentObject() {

    }

    @Test
    public void replaceExistingCOG() {

    }

    @Test 
    public void replaceNonexistentCOG() {

    }
    
    @Test
    public void noRegisteredCOGs() {
	Router router = new DefaultRouter(node1);
	assertTrue("there exists registered cogs", router.getRegisteredCOGs().isEmpty());
    }

    @Test
    public void noRegisteredObjects() {
	Router router = new DefaultRouter(node1);
	assertTrue("there exists registered objects", router.getRegisteredObjects().isEmpty());
    }

}
