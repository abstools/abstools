package org.abs_models.backend.java.lib.net;

import java.util.HashMap;
import java.util.Map;

public class NetworkGraph {
    public static class AttributeStore {
        private Map<String,String> attributes;
        public void addAttribute(String key, String value) {
            if (attributes == null) {
                attributes = new HashMap<>();
            }
            attributes.put(key,value);
        }

        public String getAttribute(String key) {
            return attributes.get(key);
        }

        public void putAll(AttributeStore s) {
            attributes.putAll(s.attributes);
        }
    }


    public static class GraphNode extends AttributeStore {
        public final String id;
        public GraphNode(String id) {
            this.id = id;
        }
    }


    public static class GraphEdge extends AttributeStore {
        public final GraphNode source;
        public final GraphNode target;
        public GraphEdge(GraphNode source, GraphNode target) {
            this.source = source;
            this.target = target;
        }

    }

    public GraphNode newNode(String id) {
        return new GraphNode(id);
    }

    public GraphEdge newEdge(GraphNode source, GraphNode target) {
        return new GraphEdge(source,target);
    }

}
