/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.types;

public abstract class ABSBuiltInDataType implements ABSDataType {
    public final String constructorName;

    public String toString() {
        // NOTE: this method was pushed down from
        // ABSDataType.toStringHelper; it works but might be too
        // general
        StringBuilder sb = new StringBuilder();
        sb.append(constructorName);
        Object[] args = getArgs();
        if (args.length > 0) {
            sb.append('(');
            int i = 0;
            for (Object v : args) {
                if (i > 0) {
                    sb.append(',');
                }
                sb.append(switch (v) {
                    case String s -> "\"" + s.toString() + "\"";
                    default -> v.toString();
                });
                i++;
            }
            sb.append(')');
        }
        return sb.toString();
    }

    protected ABSBuiltInDataType(String constructorName) {
        this.constructorName = constructorName;
    }

    public String getConstructorName() {
        return constructorName;
    }
}
