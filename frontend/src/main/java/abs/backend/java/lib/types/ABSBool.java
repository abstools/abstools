/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.types;

public class ABSBool extends ABSBuiltInDataType {
    public static ABSBool TRUE = new ABSBool("True", true);
    public static ABSBool FALSE = new ABSBool("False", false);

    private boolean value;

    private ABSBool(String constr, boolean v) {
        super(constr);
        this.value = v;
    }

    @Override
    public ABSBool eq(ABSValue o) {
        return ABSBool.fromBoolean(o == this);
    }

    @Override
    public ABSBool notEq(ABSValue o) {
        return ABSBool.fromBoolean(o != this);
    }

    public ABSBool gt(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!o.getClass().equals(ABSBool.class))
            return ABSBool.FALSE;
        ABSBool oi = (ABSBool) o;
        // True > False
        return this.and(oi.negate());
    }

    public ABSBool lt(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!o.getClass().equals(ABSBool.class))
            return ABSBool.FALSE;
        ABSBool oi = (ABSBool) o;
        // False < True
        return oi.and(this.negate());
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
        if (b)
            return TRUE;
        else
            return FALSE;
    }

}
