/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.ProductLine;

public class DeltaTrie {
    private Node root;
    private ProductLine pl;

    /**
     * Constructor
     */
    public DeltaTrie(SemanticErrorList errors) {
        root = new Node(errors);
        // build & store CORE type abstraction
    }

    public void setPl(ProductLine pl) {
        this.pl = pl;
    }

    // Adds a word to the Trie
    public void addWord(List<String> word) {
        System.out.print("DeltaSequence");
        root.addWord(word, 0);
        System.out.println();
    }

    /**********************************************************************************************/
    /*
     * Trie Node
     */
    private class Node {
        private Map<String, Node> children;
        private String deltaID = null;
        private boolean isValidProduct = false;
        private ProgramTypeAbstraction ta;

         // Constructor for top level root node
        public Node(SemanticErrorList errors) {
            this.children = new HashMap<String, Node>();
            this.ta = new ProgramTypeAbstraction(errors); // FIXME this should contain the core's TA, and main type error list
        }

        // Constructor for child node
        public Node(String name, ProgramTypeAbstraction ta) {
           this.children = new HashMap<String, Node>();
           this.deltaID = name;
           this.ta = ta;
        }

        protected void addWord(List<String> word, int d) {
            Node nextNode;
            System.out.print(">>>" + word.get(d));

            if (children.containsKey(word.get(d))) {
                nextNode = children.get(word.get(d));
            } else {
                ProgramTypeAbstraction nextTA = new ProgramTypeAbstraction(ta);
                nextNode = new Node(word.get(d), nextTA);

                // Apply delta to nextNode's program type abstraction
                DeltaDecl delta = pl.getModel().getDeltaDeclsMap().get(nextNode.deltaID);
                nextTA.applyDelta(delta);

                children.put(word.get(d), nextNode);
                System.out.print("*");
            }

            if (word.size() > d+1)
                nextNode.addWord(word, d+1);
            else {
                isValidProduct = true;
                // type check this product
                System.out.print(".");
            }
        }


        @Override
        public String toString() {
            return "[" + deltaID + ":" + ta.toString() + "]";
        }

        public String toString(StringBuilder s) {
            s.append(toString());
            for (Node child : children.values())
                s.append(child.toString(s));
            return s.toString();
        }

        protected void traverse() {
            for (Node child : children.values())
                child.traverse();
        }
    }
    /**********************************************************************************************/

    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        return root.toString(s);
    }

    public void traverse() {
        root.traverse();
        System.out.println();
    }


}
