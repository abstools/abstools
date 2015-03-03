/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class DeltaTrie {
    private Node root;

    /**
     * Constructor
     */
    public DeltaTrie() {
        root = new Node();
    }

     // Adds a word to the Trie
    public void addWord(List<String> word) {
        System.out.print("DeltaSequence");
        root.addWord(word, 0);
        System.out.println();
    }



    /*
     * Trie Node
     */
    private class Node {
        private Map<String, Node> children;
        private String value = null;
        private boolean isValidProduct = false;

         // Constructor for top level root node
        public Node() {
            children = new HashMap<String, Node>();
        }

         // Constructor for child node
        public Node(String s) {
           this();
           this.value = s;
        }

        protected void addWord(List<String> word, int d) {
            Node nextNode;
            System.out.print(">>>" + word.get(d));
            if (children.containsKey(word.get(d))) {
                nextNode = children.get(word.get(d));
            } else {
                nextNode = new Node(word.get(d));
                children.put(word.get(d), nextNode);
            }

            if (word.size() > d+1)
                nextNode.addWord(word, d+1);
            else
                isValidProduct = true;
        }

        /*
        protected Set<List<String>> traverse() {
            if (value == null) // core
                ;
            else

        }
         */
    }
}
