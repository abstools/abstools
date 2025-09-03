/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.types;

public interface ABSDataType extends ABSValue {

    static final Object[] NO_ARGS = new Object[0];

    default Object[] getArgs() {
        return NO_ARGS;
    }

    /**
     * Returns the iths constructor argument of this data value.
     * @param i the index of the constructor argument to return (starting from 0)
     * @return the i'th constructor argument of this data value
     * @throws IllegalArgumentException if {@code i < 0 or i >= getNumArgs()}
     */
    default Object getArg(int i) {
       if (i < 0 || i >= getNumArgs()) throw new IllegalArgumentException(i+ " is not a valid constructor argument index");
       return getArgs()[i];
    }

    /**
     * Returns the number of constructor arguments of this data value.
     * @return the number of constructor arguments of this data value
     */
    default int getNumArgs() {
       return getArgs().length;
    }

    default String getConstructorName() {
        return this.getClass().getSimpleName();
    }

}
