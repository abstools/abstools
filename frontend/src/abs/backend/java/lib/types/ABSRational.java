/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.types;

import java.math.BigInteger;

import org.apfloat.Apint;
import org.apfloat.Aprational;

/**
 * Implementation of an ABS rational
 * 
 * @author Rudi Schlatte
 * 
 * Based on ABSInteger by Jan Schäfer
 */
public class ABSRational extends ABSBuiltInDataType {

    public static final ABSRational ZERO = new ABSRational(Aprational.ZERO);
    public static final ABSRational ONE = new ABSRational(Aprational.ONE);

    protected Aprational value;

    private ABSRational(Aprational r) {
        super("");
        this.value = r;
    }

    protected ABSRational() {
        super("");
    }

    public ABSRational add(ABSRational i) {
        return fromBigInt(this.value.add(i.value));
    }

    public ABSRational subtract(ABSRational i) {
        return fromBigInt(this.value.subtract(i.value));
    }

    public ABSRational multiply(ABSRational i) {
        return fromBigInt(this.value.multiply(i.value));
    }

    public ABSRational divide(ABSRational i) {
        return fromBigInt(this.value.divide(i.value));
    }

    public ABSRational mod(ABSRational i) {
        return fromBigInt(this.value.mod(i.value));
    }

    public ABSRational negate() {
        return fromBigInt(this.value.negate());
    }

    @Override
    public ABSBool eq(ABSValue o) {
        if (o == null) return ABSBool.FALSE;
        if (!o.getClass().equals(ABSRational.class) && !o.getClass().equals(ABSInteger.class))
            return ABSBool.FALSE;
        ABSRational oi = (ABSRational) o;
        return ABSBool.fromBoolean(oi.value.compareTo(this.value) == 0);
    }

    public ABSBool gt(ABSRational i) {
        if (i == null)
            return ABSBool.FALSE;
        return ABSBool.fromBoolean(this.value.compareTo(i.value) == 1);
    }

    public ABSBool lt(ABSRational i) {
        if (i == null)
            return ABSBool.FALSE;
        return ABSBool.fromBoolean(this.value.compareTo(i.value) == -1);
    }

    public ABSBool gtEq(ABSRational i) {
        if (i == null)
            return ABSBool.FALSE;
        int res = this.value.compareTo(i.value);
        return ABSBool.fromBoolean(res == 0 || res == 1);
    }

    public ABSBool ltEq(ABSRational i) {
        if (i == null)
            return ABSBool.FALSE;
        int res = this.value.compareTo(i.value);
        return ABSBool.fromBoolean(res == 0 || res == -1);
    }

    public static ABSRational fromBigInt(BigInteger i) {
        return new ABSRational(new Apint(i));
    }

    public static ABSRational fromBigInt(Aprational i) {
        return new ABSRational(i);
    }

    public static ABSRational fromString(String value) {
        return new ABSRational(new Aprational(value));
    }

    public static ABSRational fromInt(int i) {
        switch (i) {
        case 0:
            return ZERO;
        case 1:
            return ONE;
        default:
            return fromBigInt(new Apint(i));
        }
    }

    public static ABSRational fromLong(long l) {
        if (l == 0) return ZERO;
        if (l == 1) return ONE;
        return fromBigInt(new Apint(l));
    }

    public int toInt() {
        return value.intValue();
    }

    @Override
    public String toString() {
        return value.toString();
    }

    public ABSInteger truncate() {
        return ABSInteger.fromBigInt(value.truncate());
    }

}
