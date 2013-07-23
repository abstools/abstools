/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.types;

public class ABSString extends ABSBuiltInDataType {
    public static final ABSString EMPTY = new ABSString("");

    private final String value;

    private ABSString(String s) {
        super("");
        this.value = s;
    }

    public ABSString add(ABSString s) {
        return fromString(value + s.value);
    }

    @Override
    public ABSBool eq(ABSValue o) {
        if (!super.eq(o).toBoolean())
            return ABSBool.FALSE;
        ABSString s = (ABSString) o;
        return ABSBool.fromBoolean(this.value.equals(s.value));
    }

    public ABSBool gt(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!o.getClass().equals(ABSString.class))
            return ABSBool.FALSE;
        ABSString oi = (ABSString) o;
        return ABSBool.fromBoolean(this.value.compareTo(oi.value) > 0);
    }

    public ABSBool lt(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!o.getClass().equals(ABSString.class))
            return ABSBool.FALSE;
        ABSString oi = (ABSString) o;
        return ABSBool.fromBoolean(this.value.compareTo(oi.value) < 0);
    }

    public ABSBool gtEq(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!o.getClass().equals(ABSString.class))
            return ABSBool.FALSE;
        ABSString oi = (ABSString) o;
        int res = this.value.compareTo(oi.value);
        return ABSBool.fromBoolean(res == 0 || res == 1);
    }

    public ABSBool ltEq(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!o.getClass().equals(ABSString.class))
            return ABSBool.FALSE;
        ABSString oi = (ABSString) o;
        int res = this.value.compareTo(oi.value);
        return ABSBool.fromBoolean(res == 0 || res == -1);
    }


    public static ABSString fromString(String s) {
        if (s.isEmpty())
            return EMPTY;
        return new ABSString(s);
    }

    public String getString() {
        return value;
    }

    public ABSInteger strlen() {
        return ABSInteger.fromInt(value.length());
    }

    public ABSString substr(ABSInteger from, ABSInteger length) {
        return fromString(value.substring(from.toInt(), from.toInt() + length.toInt()));
    }

    @Override
    public String toString() {
        return "\"" + value + "\"";
    }
}
