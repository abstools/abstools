/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.types;

public class ABSBool extends ABSBuiltInDataType {
    public final static ABSBool TRUE = new ABSBool("True", true);
    public final static ABSBool FALSE = new ABSBool("False", false);

    private boolean value;

    private ABSBool(String constr, boolean v) {
        super(constr);
        this.value = v;
    }

    public ABSBool negate() {
        return fromBoolean(!value);
    }

    public ABSBool and(ABSBool b) {
        return fromBoolean(value && b.value);
    }

    public ABSBool or(ABSBool b) {
        return fromBoolean(value || b.value);
    }

    public boolean toBoolean() {
        return value;
    }

    public static ABSBool fromBoolean(boolean b) {
        return b ? TRUE : FALSE;
    }

    public Object toJson() {
        return toBoolean();
    }

}
