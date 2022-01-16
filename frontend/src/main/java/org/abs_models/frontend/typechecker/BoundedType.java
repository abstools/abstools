/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import org.abs_models.frontend.variablechecker.ModelFamilySignature;

public class BoundedType extends Type {
    private Type boundType;
    private boolean gaveOutHashCode = false; // don't give out different values
    private int hashCode = 42;

    public BoundedType() {

    }

    public void bindTo(Type t) {
        if (t == this) {
            assert false : "Trying to bind BoundedType to itself";
            return;
        }
        while (t.isBoundedType()) {
            BoundedType bt = (BoundedType)t;
            if (bt.hasBoundType()) {
                t = bt.getBoundType();
            } else {
                assert false : "Trying to bind to a BoundedType that is itself unbound";
                return;         // TODO: how should we handle this?
            }
        }
        boundType = t;
    }

    public Type getBoundType() {
        return boundType;
    }

    @Override
    public Object getMetaData(Object key) {
        if (hasBoundType()) {
            return boundType.getMetaData(key);
        }
        return super.getMetaData(key);
    }
    
    @Override
    public void addMetaData(Object key, Object value) {
        if (hasBoundType()) {
            boundType.addMetaData(key,value);
        } else  {
            super.addMetaData(key, value);
        }
    }

    @Override
    public boolean varIsAssignableTo(Type t, ModelFamilySignature signature) {
        return this.isAssignableTo(t);
    }

    @Override
    public boolean isAssignableTo(Type t) {
        if (hasBoundType())
            return boundType.isAssignableTo(t);

        while (t.isBoundedType()) {
            BoundedType bt = (BoundedType)t;
            if (bt.hasBoundType()) {
                t = bt.getBoundType();
            } else {
                // We're comparing two unbound types -- do the minimally
                // correct thing
                // (https://github.com/abstools/abstools/issues/247)
                return false;
            }
        }
        // minimally invasive change wrt original code: arguably we
        // should not have side effects in this method.
        // TODO: figure out how to treat this case instead
        if (t != this) bindTo(t);
        return true;
    }

    public int hashCode() {
        if (!gaveOutHashCode && hasBoundType()) hashCode =  boundType.hashCode();
        gaveOutHashCode = true;
        return hashCode;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof Type))
            return false;

        if (this == o)
            return true;

        if (hasBoundType())
            return boundType.equals(o);

        return true;
    }

    @Override
    public boolean isBoundedType() {
        return true;
    }

    public boolean hasBoundType() {
        return boundType != null;
    }

    @Override
    public String toString() {
        if (hasBoundType()) {
            return boundType.toString();
        }
        return "Unbound Type";
    }
    
    @Override
    public String getSimpleName() {
        if (hasBoundType()) {
            return boundType.getSimpleName();
        }
        return "Unbound Type";
    }

    @Override
    public boolean canBeBoundTo(Type t) {
        if (!hasBoundType()) {
            // minimally invasive change wrt original code: arguably we should
            // not have side effects in this method.
            if (t != this) bindTo(t); // FIXME: is this correct?  Doesn't look so
            return true;
        }
        return false;
    }

    @Override
    public Type copy() {
        BoundedType copy = new BoundedType();
        copy.boundType = boundType;
        return copy;
    }
    
    public Type fullCopy() {
        if (hasBoundType()) {
            BoundedType copy = (BoundedType) copy();
            copy.boundType.metaData.putAll(boundType.metaData);
            return copy;
        } else
            return super.fullCopy();
    }

}
