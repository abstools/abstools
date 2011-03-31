/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

public enum AdaptDirection {
    FROM, TO;
    
    public boolean isFrom() {
        return this == FROM;
    }
    
    public boolean isTo() {
        return this == TO;
    }
}