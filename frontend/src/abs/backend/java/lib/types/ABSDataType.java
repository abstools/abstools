/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.types;

import abs.backend.java.lib.expr.PatternBinding;
import abs.backend.java.lib.expr.PatternConstructor;

public abstract class ABSDataType implements ABSValue {

    public ABSBool eq(ABSValue other) {
        if (other == null || other.getClass() != this.getClass())
            return ABSBool.FALSE;
        return ABSBool.TRUE;
    }

    public ABSBool notEq(ABSValue other) {
        return this.eq(other).negate();
    }

    public ABSBool gtEq(ABSValue o) {
        if (this.eq(o).toBoolean())
            return ABSBool.TRUE;
        else
            return this.gt(o);
    }

    public ABSBool ltEq(ABSValue o) {
        if (this.eq(o).toBoolean())
            return ABSBool.TRUE;
        else
            return this.lt(o);
    }

    public ABSBool gt(ABSValue o) {
        if (o instanceof ABSDataType) {
            ABSDataType other = (ABSDataType)o;
            int constructorComparison = this.getConstructorName().compareTo(other.getConstructorName());
            if (constructorComparison > 0) {
                return ABSBool.TRUE;
            } else if (constructorComparison == 0) {
                for (int i = 0; i < getNumArgs(); i++) {
                    if (getArg(i).gt(other.getArg(i)).equals(ABSBool.TRUE))
                        return ABSBool.TRUE;
                }
                return ABSBool.FALSE;
            } else {
                return ABSBool.FALSE;
            }
            // return ABSBool.fromBoolean(this.pid > ((ABSProcess)o).getPid());
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    public ABSBool lt(ABSValue o) {
        if (o instanceof ABSDataType) {
            ABSDataType other = (ABSDataType)o;
            int constructorComparison = this.getConstructorName().compareTo(other.getConstructorName());
            if (constructorComparison > 0) {
                return ABSBool.FALSE;
            } else if (constructorComparison == 0) {
                for (int i = 0; i < getNumArgs(); i++) {
                    if (getArg(i).lt(other.getArg(i)).equals(ABSBool.TRUE))
                        return ABSBool.TRUE;
                }
                return ABSBool.FALSE;
            } else {
                return ABSBool.TRUE;
            }
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    public abstract boolean match(PatternConstructor p, PatternBinding b);

    private static final ABSValue[] NO_ARGS = new ABSValue[0];

    protected ABSValue[] getArgs() {
        return NO_ARGS;
    }

    /**
     * Returns the iths constructor argument of this data value.
     * @param i the index of the constructor argument to return (starting from 0)
     * @return
     * @throws IllegalArgumentException if i < 0 or i >= getNumArgs() 
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
    
    /**
     * Whether this data type is a built-in data type
     */
    public boolean isBuiltIn() {
        return false;
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

    @Override
    public boolean isDataType() {
        return true;
    }

    @Override
    public boolean isReference() {
        return false;
    }

}
