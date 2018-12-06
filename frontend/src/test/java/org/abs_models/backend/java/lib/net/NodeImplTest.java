/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

import org.abs_models.backend.java.lib.runtime.ABSObject;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import static org.junit.Assert.*;
import static org.easymock.EasyMock.*;

import org.abs_models.backend.java.lib.runtime.*;
import org.abs_models.backend.java.lib.net.msg.ObjectTargetMsg;
import org.abs_models.backend.java.lib.net.msg.ObjectMsg;
import org.abs_models.backend.java.lib.net.msg.COGMsg;
import org.abs_models.backend.java.lib.net.msg.TableMsg;
import org.abs_models.backend.java.lib.net.msg.MsgQueue;

public class NodeImplTest {

    private NetNode currentNode;
    private NetNode otherNode;
    private ABSObject object;
    private NetCOG cog;
    private Router currentRouter;
    private Router otherRouter;
    private ArcImpl arc;
    private MsgQueue queue;

    @Before
    public void setUp() {
	currentRouter = createMock(Router.class);
	otherRouter = createMock(Router.class);

	currentNode = new NodeImpl(0, currentRouter);
	otherNode = new NodeImpl(1, otherRouter);

	arc = createMock(ArcImpl.class);

	Map<NetNode, ArcImpl> outArcs = new HashMap<>();
	outArcs.put(otherNode, arc);
	currentNode.addOutArcs(outArcs);

	List<ArcImpl> inArcs = new ArrayList<>();
	inArcs.add(arc);
	otherNode.addInArcs(inArcs);

        queue = createMock(MsgQueue.class);

	object = createMock(ABSObject.class);
	cog = createMock(NetCOG.class);
    }

    @Test
    public void getId() {
	assertEquals("must have id 0", 0, currentNode.getId());
    }


    @Test
    @Ignore ("Failing test on unmaintained code for undocumented feature")
    public void processObjectTargetMsgOnNode() {
	ObjectTargetMsg msg = createMock(ObjectTargetMsg.class);
	currentNode.registerObject(object);

	expect(msg.getTarget()).andReturn(object);
	expect(msg.getCOG()).andReturn(cog);

	cog.processMsg(msg);
	expectLastCall();

	replay(msg);
	replay(object);
	replay(cog);

	currentNode.processMsg(msg);

	verify(msg);
	verify(object);
	verify(cog);
    }

    @Test
    public void processObjectTargetMsgNotOnNode() {
	ObjectTargetMsg msg = createMock(ObjectTargetMsg.class);

	expect(msg.getTarget()).andReturn(object);
	expect(currentRouter.getNextNode(msg)).andReturn(otherNode);
	expect(arc.getQueue()).andReturn(queue);

	queue.enqueue(msg);
	expectLastCall();

	replay(msg);
	replay(currentRouter);
	replay(arc);
	replay(queue);

	currentNode.processMsg(msg);

	verify(msg);
	verify(currentRouter);
	verify(arc);
	verify(queue);
    }

    @Test
    @Ignore ("Failing test on unmaintained code for undocumented feature")
    public void processObjectMsgOnNode() {
	ObjectMsg msg = createMock(ObjectMsg.class);
	currentNode.registerCOG(cog);

	expect(msg.getObject()).andReturn(object);
	expect(msg.getCOG()).andReturn(cog);

	currentRouter.replace(object, currentNode, 0);
	expectLastCall();

	replay(msg);
	replay(object);
	replay(currentRouter);

	currentNode.processMsg(msg);

	assertTrue("object must be at node", currentNode.getRegisteredObjects().contains(object));
	verify(msg);
	verify(cog);
	verify(currentRouter);
    }

    @Test
    @Ignore ("Failing test on unmaintained code for undocumented feature")
    public void processObjectMsgNotOnNode() {
	ObjectMsg msg = createMock(ObjectMsg.class);

	expect(msg.getObject()).andReturn(object);
	expect(msg.getCOG()).andReturn(cog);
	expect(currentRouter.getNextNode(msg)).andReturn(otherNode);
	expect(arc.getQueue()).andReturn(queue);

	queue.enqueue(msg);
	expectLastCall();

	replay(msg);
	replay(object);
	replay(currentRouter);
	replay(arc);
	replay(queue);

	currentNode.processMsg(msg);

	verify(msg);
	verify(object);
	verify(currentRouter);
	verify(arc);
	verify(queue);
    }

    @Test
    @Ignore ("Failing test on unmaintained code for undocumented feature")
    public void processCOGMsg() {
	COGMsg msg = createMock(COGMsg.class);

	expect(msg.getCOG()).andReturn(cog);

	cog.setNode(currentNode);
	expectLastCall();

	currentRouter.replace(cog, currentNode, 0);
	expectLastCall();

	replay(msg);
	replay(cog);
	replay(currentRouter);

	currentNode.processMsg(msg);

	assertTrue("cog must be at node", currentNode.getRegisteredCOGs().contains(cog));
	verify(msg);
	verify(cog);
	verify(currentRouter);
    }

    @Test
    @Ignore ("Failing test on unmaintained code for undocumented feature")
    public void processTableMsg() {
	TableMsg msg = createMock(TableMsg.class);

	expect(msg.getRouter()).andReturn(otherRouter);

	currentRouter.update(otherNode, otherRouter);
	expectLastCall();

	replay(msg);
	replay(currentRouter);

	currentNode.processMsg(msg);

	verify(msg);
	verify(currentRouter);
    }

}
