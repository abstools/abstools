/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.types;

public class ABSUnit extends ABSBuiltInDataType {
    public final static ABSUnit UNIT = new ABSUnit();

    private ABSUnit() {
        super("Unit");
    }

    public ABSBool gt(ABSValue o) {
        return ABSBool.FALSE;
    }

    public ABSBool lt(ABSValue o) {
        return ABSBool.FALSE;
    }

    public Object toJson() {
        return "Unit";
    }
}
