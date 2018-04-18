/** 
 * Copyright (c) 2018, the ABS language team. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.types;

public class ABSDouble extends ABSBuiltInDataType {

    private final double value;

    private ABSDouble(double d) {
        super("");
        this.value = d;
    }

    public ABSDouble add(ABSDouble s) {
        return fromDouble(value + s.value);
    }

    public ABSDouble subtract(ABSDouble i) {
        return fromDouble(this.value - i.value);
    }

    public ABSDouble multiply(ABSDouble i) {
        return fromDouble(this.value * i.value);
    }

    public ABSDouble divide(ABSDouble i) {
        return fromDouble(this.value / i.value);
    }

    public ABSDouble divide(double i) {
        return fromDouble(this.value / i);
    }

    public ABSDouble mod(ABSDouble i) {
        return fromDouble(this.value % i.value);
    }

    public ABSDouble negate() {
        return fromDouble(- this.value);
    }

    @Override
    public ABSBool eq(ABSValue o) {
        if (!super.eq(o).toBoolean())
            return ABSBool.FALSE;
        ABSDouble s = (ABSDouble) o;
        return ABSBool.fromBoolean(this.value == s.value);
    }

    public ABSBool gt(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!o.getClass().equals(ABSDouble.class))
            return ABSBool.FALSE;
        ABSDouble oi = (ABSDouble) o;
        return ABSBool.fromBoolean(this.value > oi.value);
    }

    public ABSBool lt(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!o.getClass().equals(ABSDouble.class))
            return ABSBool.FALSE;
        ABSDouble oi = (ABSDouble) o;
        return ABSBool.fromBoolean(this.value < oi.value);
    }

    public ABSBool gtEq(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!o.getClass().equals(ABSDouble.class))
            return ABSBool.FALSE;
        ABSDouble oi = (ABSDouble) o;
        return ABSBool.fromBoolean(this.value >= oi.value);
    }

    public ABSBool ltEq(ABSValue o) {
        if (o == null)
            return ABSBool.FALSE;
        if (!o.getClass().equals(ABSDouble.class))
            return ABSBool.FALSE;
        ABSDouble oi = (ABSDouble) o;
        return ABSBool.fromBoolean(this.value <= oi.value);
    }

    public static ABSDouble fromString(String value) {
        return new ABSDouble(Double.parseDouble(value));
    }

    public static ABSDouble fromDouble(double d) {
        return new ABSDouble(d);
    }

    public double getDouble() {
        return value;
    }

    @Override
    public String toString() {
        return Double.toString(this.value);
    }
}
