/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

public abstract class ReferenceType extends Type {
    
    @Override
    public boolean isReferenceType() {
        return true;
    }
}
