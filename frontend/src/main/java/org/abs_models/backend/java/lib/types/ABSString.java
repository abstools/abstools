/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.types;

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
        return value;
    }

}
