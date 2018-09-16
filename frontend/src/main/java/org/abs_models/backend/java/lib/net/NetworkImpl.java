/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.net;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class NetworkImpl implements Network {

    private final List<NetNode> nodes = new ArrayList<>();
    private final List<Arc> arcs = new ArrayList<>();

    @Override
    public Iterable<NetNode> getAllNodes() {
        return Collections.unmodifiableList(nodes);
    }

    @Override
    public Iterable<Arc> getAllArcs() {
        return Collections.unmodifiableList(arcs);
    }

    @Override
    public NetNode getStartNode() {
        return nodes.get(0);
    }

    public void addNode(NetNode n) {
        nodes.add(n);
    }

}
