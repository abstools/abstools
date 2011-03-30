/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.util.ArrayList;

public class SemanticErrorList extends ArrayList<SemanticError> {
    public SemanticError getFirst() {
        return get(0);
    }
}
