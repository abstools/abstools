/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import abs.backend.java.lib.net.msg.COGMsg;
import abs.backend.java.lib.net.msg.CallMsg;
import abs.backend.java.lib.net.msg.Msg;
import abs.backend.java.lib.net.msg.ObjectMsg;
import abs.backend.java.lib.net.msg.ObjectTargetMsg;
import abs.backend.java.lib.net.msg.PromiseMsg;
import abs.backend.java.lib.runtime.ABSObject;

/**
 * Representing a Node in the ABS network
 *
 * @author Jan Schäfer
 *
 */
public class NodeImpl implements NetNode {
    private final int id;
    private final Router router;
    private final Map<NetNode,ArcImpl> outArcs = new HashMap<>();
    private final List<ArcImpl> inArcs = new ArrayList<>();
    private final Set<ABSObject> objects = new HashSet<>();
    private final Set<NetCOG> cogs = new HashSet<>();
    private final Random random = new Random();

    public NodeImpl(int id) {
        this.id = id;
	router = new DefaultRouter(this);
    }

    public NodeImpl(int id, Router router) {
	this.id = id;
	this.router = router;
    }

    @Override
    public void addInArcs(List<ArcImpl> arcs) {
        inArcs.addAll(arcs);
    }

    @Override
    public void addOutArcs(Map<NetNode, ArcImpl> arcs) {
	outArcs.putAll(arcs);
    }

    public synchronized boolean processNextMsg() {
        List<ArcImpl> shuffledList = new ArrayList<>(inArcs);
        Collections.shuffle(shuffledList);
        for (ArcImpl arc : shuffledList) {
            if (!arc.getQueue().isEmpty()) {
                Msg m = arc.getQueue().dequeue();
                processMsg(m);
                return true;
            }
        }
        return false;
    }

    public void sendMsg(Msg m) {

    }

    @Override
    public synchronized void processMsg(Msg m) {
        if (m instanceof ObjectTargetMsg) {
            ObjectTargetMsg otm = (ObjectTargetMsg) m;
            if (objects.contains(otm.getTarget())) {
                NetCOG cog = (NetCOG) otm.getTarget().getCOG();
                cog.processMsg(m);
            } else {
                routeAway(m);
            }
        } else if (m instanceof COGMsg) {
            COGMsg cm = (COGMsg) m;
	    NetCOG cog = cm.getCOG();
            cog.setNode(this);
            cogs.add(cog);
        } else if (m instanceof ObjectMsg) {
            ObjectMsg om = (ObjectMsg) m;
	    ABSObject object = om.getObject();
            NetCOG cog = (NetCOG) object.getCOG();
            if (cogs.contains(cog)) {
                objects.add(object);
            } else {
                routeAway(m);
            }
        }
    }

    private void routeAway(Msg m) {
        outArcs.get(getNextNode(m)).getQueue().enqueue(m);
    }

    private NetNode getNextNode(Msg m) {
        NetNode node = router.getNextNode(m);
        if (node == null) {
            node = defaultRoute();
        }
        return node;
    }

    @Override
    public NetNode defaultRoute() {
        return null;
    }

    @Override
    public void performStep() {
        if (processNextMsg())
            return;

        if (migrateGroup())
            return;

        if (migrateObject())
            return;
    }

    @Override
    public synchronized void registerObject(ABSObject absObject) {
        objects.add(absObject);
    }

    @Override
    public synchronized void registerCOG(NetCOG cog) {
        cogs.add(cog);
    }

    @Override
    public Set<ABSObject> getRegisteredObjects() {
	return objects;
    }

    @Override
    public Set<NetCOG> getRegisteredCOGs() {
	return cogs;
    }

    @Override
    public int getId() {
	return id;
    }

    private synchronized boolean migrateObject() {
        for (ABSObject o : objects) {
            NetCOG cog = (NetCOG)o.getCOG();
            if (!cogs.contains(cog)) {
                NetNode node = router.getRouteEntry(cog).getNextNode();
                objects.remove(o);
                outArcs.get(node).getQueue().enqueue(new ObjectMsg(o));
                return true;
            }
        }
        return false;
    }

    private synchronized boolean migrateGroup() {
        if (random.nextBoolean())
            return false;

        if (cogs.isEmpty())
            return false;

        NetCOG cog = cogs.iterator().next();
        cogs.remove(cog);

        ArcImpl arc = outArcs.values().iterator().next();
        arc.getQueue().enqueue(new COGMsg(cog));

        return true;
    }

}
