/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.util.ArrayList;

import abs.frontend.typechecker.TypeCheckerException;

@SuppressWarnings("serial")
public class SemanticConditionList extends ArrayList<SemanticError> {

    public SemanticConditionList() {}
    
    public SemanticConditionList(TypeCheckerException e) {
        add(e);
    }

    public boolean containsErrors() {
        // Prepare for this list to contain warnings as well, which should not
        // abort the compilation
        return !isEmpty();
    }

    public SemanticError getFirst() {
        return get(0);
    }

    public void add(TypeCheckerException e) {
        add(e.getTypeError());
    }
    
    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        boolean first = true;
        for(SemanticError e : this) {
            if (!first)
                buf.append(',');
            buf.append(e.toString());
            first = false;
        }
        return buf.toString();
    }
}
