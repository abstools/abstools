/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.types;

public abstract class ABSDataType implements ABSValue {

    private static final ABSValue[] NO_ARGS = new ABSValue[0];

    protected ABSValue[] getArgs() {
        return NO_ARGS;
    }

    /**
     * Returns the iths constructor argument of this data value.
     * @param i the index of the constructor argument to return (starting from 0)
     * @return the i'th constructor argument of this data value
     * @throws IllegalArgumentException if {@code i < 0 or i >= getNumArgs()}
     */
    public ABSValue getArg(int i) {
       if (i < 0 || i >= getNumArgs()) throw new IllegalArgumentException(i+ " is not a valid constructor argument index");
       return getArgs()[i];
    }

    /**
     * Returns the number of constructor arguments of this data value.
     * @return the number of constructor arguments of this data value
     */
    public int getNumArgs() {
       return getArgs().length;
    }

    public String getConstructorName() {
        return this.getClass().getSimpleName();
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        toStringBuilder(sb);
        return sb.toString();
    }

    private void toStringBuilder(StringBuilder sb) {
        sb.append(getConstructorName());
        ABSValue[] args = getArgs();
        if (args.length > 0) {
            sb.append('(');
            int i = 0;
            for (ABSValue v : args) {
                if (i > 0) {
                    sb.append(',');
                }

                sb.append(v.toString());
                i++;
            }
            sb.append(')');
        }
    }

}
