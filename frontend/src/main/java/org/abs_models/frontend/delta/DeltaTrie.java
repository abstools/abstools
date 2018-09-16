/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.base.Strings;

import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Product;
import org.abs_models.frontend.ast.Model;

/*
 * A DeltaTrie (product family generation trie, Damiani & Schaefer 2012) is a representation of all possible products.
 * Type checking all possible product variants amounts to traversing the trie and computing the type abstractions
 * for all (intermediate) products associated to the nodes of the trie.
 *
 */
public class DeltaTrie {
    private final Node root;
    private final Model model;

    /**
     * Constructor
     */
    public DeltaTrie(Model model, SemanticConditionList errors) {
        this.model = model;
        root = new Node(errors);
    }

    // Adds a word to the Trie
    public void addWord(List<String> word, Product product) {
        if (word.size() == 0) // no deltas
            root.isValidProduct = true;
        else
            root.addWord(word, product, 0);
    }

    /**********************************************************************************************/
    /*
     * Trie node
     */
    class Node {
        private final Map<String, Node> children;
        private String deltaID = null;
        private boolean isValidProduct = false;
        private final ProgramAbstraction ta;

        // Constructor for top level root node
        public Node(SemanticConditionList errors) {
            this.deltaID = "core";
            this.children = new HashMap<>();
            this.ta = new ProgramAbstraction(errors);
            model.buildCoreAbstraction(ta);
        }

        // Constructor for child node
        public Node(String name, ProgramAbstraction ta) {
            this.children = new HashMap<>();
            this.deltaID = name;
            this.ta = ta;
        }

        /** Add a word to the trie
         *
         * @param word     Non-empty List of delta names
         * @param product  The SPL product that this word represents (or null if none)
         * @param d        Index of List element to start with (for recursive invocation)
         */
        protected void addWord(List<String> word, Product product, int d) {
            Node nextNode;

            //System.out.print(">>>" + word.get(d));

            if (children.containsKey(word.get(d))) {
                // node already exists
                nextNode = children.get(word.get(d));
            } else {
                ProgramAbstraction nextTA = new ProgramAbstraction(ta);
                nextNode = new Node(word.get(d), nextTA);

                // Apply delta to nextNode's program type abstraction
                DeltaDecl delta = model.getDeltaDeclsMap().get(nextNode.deltaID);
                nextTA.applyDelta(delta, product);

                children.put(word.get(d), nextNode);
                //System.out.print("*");
            }

            if (word.size() > d+1)
                nextNode.addWord(word, product, d+1);
            else {
                nextNode.isValidProduct = true;
                // TODO type check this product (???)
                //System.out.println(".");
            }
        }


        // Getters
        public Map<String, Node> getChildren() {
            return children;
        }

        public String getDeltaID() {
            return deltaID;
        }

        public boolean isValidProduct() {
            return isValidProduct;
        }

        public ProgramAbstraction getProgramAbstraction() {
            return ta;
        }

        /*
        @Override
        public String toString() {
            return toString(new StringBuilder(), false);
        }


        public String toString(StringBuilder s, boolean printTA) {
            s.append("Delta: " + deltaID + ".  " + "Valid Product: " + isValidProduct + "\n");
            if (printTA)
                s.append(ta.toString());
            for (Node child : children.values())
                child.toString(s, printTA);
            return s.toString();
        }
         */

        protected int height() {
            int h = 0;
            for (Node child : children.values())
                h = Math.max(h, child.height());
            return 1 + h;
        }

        protected void traversePreorder(StringBuilder s, int level) {
            s.append(Strings.repeat("|   ", level));
            s.append("|---");
            s.append(getDeltaID() + (isValidProduct() ? "\u2713" : "") + "\n");
            for (Node child : getChildren().values())
                child.traversePreorder(s, level+1);
        }

    }

    /*
     * Convenience methods
     */

    public Node getRoot() {
        return root;
    }

    /*
     * Height of the tree (1=only root node)
     */
    public int height() {
        return root.height();
    }

    /*
     * Return a textual representation of the tree
     * A checkmark next to a node means it represents a valid product.
     */
    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        root.traversePreorder(s, 0);
        return s.toString();
    }

}
