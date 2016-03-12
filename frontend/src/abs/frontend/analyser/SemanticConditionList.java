/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.util.ArrayList;
import java.util.Iterator;

import abs.frontend.typechecker.TypeCheckerException;

@SuppressWarnings("serial")
public class SemanticConditionList implements Iterable<SemanticError> {

    ArrayList<SemanticError> contents = new ArrayList<SemanticError>();

    public SemanticConditionList() {}
    
    public SemanticConditionList(TypeCheckerException e) {
        add(e);
    }

    // Iterable protocol
    public Iterator<SemanticError> iterator() {
        return contents.iterator();
    }

    public boolean containsErrors() {
        // Prepare for this list to contain warnings as well, which should not
        // abort the compilation
        return !contents.isEmpty();
    }

    public int getErrorCount() {
        return contents.size();
    }

    public SemanticError getFirstError() {
        return contents.get(0);
    }

    public boolean add(TypeCheckerException e) {
        return contents.add(e.getTypeError());
    }
    
    public boolean add(SemanticError e) {
        return contents.add(e);
    }

    public boolean addAll(SemanticConditionList l) {
        return contents.addAll(l.contents);
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        boolean first = true;
        for(SemanticError e : contents) {
            if (!first)
                buf.append(',');
            buf.append(e.toString());
            first = false;
        }
        return buf.toString();
    }
}
