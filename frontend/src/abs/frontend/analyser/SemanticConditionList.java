/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.util.ArrayList;
import java.util.Iterator;

import abs.frontend.typechecker.TypeCheckerException;

@SuppressWarnings("serial")
public class SemanticConditionList implements Iterable<SemanticCondition> {

    ArrayList<SemanticCondition> contents = new ArrayList<SemanticCondition>();
    boolean containsErrors = false;

    public SemanticConditionList() {}
    
    public SemanticConditionList(TypeCheckerException e) {
        add(e);
    }

    // Iterable protocol
    public Iterator<SemanticCondition> iterator() {
        return contents.iterator();
    }

    public boolean containsErrors() {
        return containsErrors;
    }

    public int getErrorCount() {
        int count = 0;
        for (SemanticCondition c : contents) {
            if (c.isError()) count = count + 1;
        }
        return count;
    }

    public SemanticCondition getFirstError() {
        for (SemanticCondition c : contents) {
            if (c.isError()) return c;
        }
        return null;
    }

    public boolean add(TypeCheckerException e) {
        containsErrors = true;
        return contents.add(e.getTypeError());
    }
    
    public boolean add(SemanticCondition e) {
        if (e.isError()) containsErrors = true;
        return contents.add(e);
    }

    public boolean addAll(SemanticConditionList l) {
        if (!containsErrors) containsErrors = l.containsErrors();
        return contents.addAll(l.contents);
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        boolean first = true;
        for(SemanticCondition e : contents) {
            if (!first)
                buf.append(',');
            buf.append(e.toString());
            first = false;
        }
        return buf.toString();
    }
}
