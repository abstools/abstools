/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.types;

public interface ABSValue extends ABSType {
    ABSBool eq(ABSValue o);

    ABSBool notEq(ABSValue o);

    ABSBool gt(ABSValue other);

    ABSBool lt(ABSValue other);

    ABSBool gtEq(ABSValue other);

    ABSBool ltEq(ABSValue other);

    java.lang.Object toJson();

}
