/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

public final class NullType extends ReferenceType {
    public final static NullType INSTANCE = new NullType();

    private NullType() {

    }

    @Override
    public boolean isNullType() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof NullType;
    }
    
    @Override
    public boolean isAssignable(Type t) {
        if (super.isAssignable(t))
            return true;
        return t instanceof ReferenceType;
    }

    @Override
    public String toString() {
        return "NullType";
    }

    @Override
    public String getModuleName() {
        return null;
    }

    @Override
    public String getSimpleName() {
        return null;
    }

    @Override
    public Type copy() {
        return new NullType();
    }
}
