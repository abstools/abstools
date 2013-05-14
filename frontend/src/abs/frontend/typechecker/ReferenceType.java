/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.List;

public abstract class ReferenceType extends Type {
    
    @Override
    public boolean isReferenceType() {
        return true;
    }

    @Override
    public DataTypeUse toUse() {
        return new DataTypeUse(getSimpleName(), new List());
    }
}
