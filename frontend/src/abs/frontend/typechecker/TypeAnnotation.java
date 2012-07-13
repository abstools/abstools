/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

import abs.frontend.ast.Annotation;
import abs.frontend.ast.PureExp;

public class TypeAnnotation {
    private DataTypeType type;
    private PureExp value;
    
    public TypeAnnotation(Annotation a) {
        assert a.getType() instanceof DataTypeType;
        value = a.getValue();
        type = (DataTypeType) a.getType();
    }
    
    public DataTypeType getType() {
        return type;
    }
    
    public PureExp getValue() {
        return value;
    }
    
    public String toString() {
        String type = getType() == null ? "" : getType().toString()+":";
        return "["+ type + getValue().toString()+"]";
    }

}
