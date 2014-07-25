/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import abs.frontend.ast.DeltaDecl;

public class DeltaModellingException extends RuntimeException {

    private static final long serialVersionUID = 1L;
    private final DeltaDecl delta;

    public DeltaModellingException(DeltaDecl d, String msg) {
        super(d == null ? msg : "Delta " + d.getName()+":"+msg);
        delta = d;
    }

    public DeltaModellingException(String msg) {
        this(null,msg);
    }
    
    public DeltaDecl getDelta() {
        return delta;
    }
}
