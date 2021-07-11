package org.abs_models.backend.rvsdg.core;

import java.util.HashSet;
import java.util.Set;

public class Region {
    public final Set<Node> nodes = new HashSet<>();

    public void addNode(Node node) {
        nodes.add(node);
    }

    public void removeNode(Node node) {
        nodes.remove(node);
    }
}
