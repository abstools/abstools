/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.util.ArrayList;

import abs.frontend.typechecker.TypeCheckerException;

@SuppressWarnings("serial")
public class SemanticErrorList extends ArrayList<SemanticError> {

    public SemanticErrorList() {}
    
    public SemanticErrorList(TypeCheckerException e) {
        add(e);
    }

    public SemanticError getFirst() {
        return get(0);
    }

    public void add(TypeCheckerException e) {
        add(e.getTypeError());
    }
}
