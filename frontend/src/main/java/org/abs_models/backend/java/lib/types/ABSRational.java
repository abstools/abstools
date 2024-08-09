/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.types;

import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.ApintMath;
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
        return fromAprational(this.value.add(i.value));
    }

    public ABSRational subtract(ABSRational i) {
        return fromAprational(this.value.subtract(i.value));
    }

    public ABSRational multiply(ABSRational i) {
        return fromAprational(this.value.multiply(i.value));
    }

    public ABSRational divide(ABSRational i) {
        return fromAprational(this.value.divide(i.value));
    }

    public ABSRational mod(ABSRational i) {
        return fromAprational(this.value.mod(i.value));
    }

    public ABSRational negate() {
        return fromAprational(this.value.negate());
    }

    public static ABSRational fromAprational(Aprational i) {
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
            return fromAprational(new Apint(i));
        }
    }

    public static ABSRational fromLong(long l) {
        if (l == 0) return ZERO;
        if (l == 1) return ONE;
        return fromAprational(new Apint(l));
    }

    public static ABSRational fromDouble(double v) {
        // This method is a bit baroque;
        // http://apfloat.org/apfloat_java/docs/org/apfloat/Aprational.html#%3Cinit%3E(double)
        // "new Aprational(0.1) won't result in 1/10" -- but we do want to go
        // via the printed representation

        if (v == 0.0) { return ZERO; }
        if (v == 1.0) { return ONE; }
        if (v == Math.rint(v)) {
            // https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#rint-double-
            // "If the argument value is already equal to a mathematical
            // integer, then the result is the same as the argument."
            return new ABSRational(new Aprational(v));
        }
        String doubles = new Apfloat(v).toString(true);
        long length_of_fraction = doubles.length() - (doubles.indexOf('.') + 1);
        Apint den = ApintMath.pow(new Apint(10), length_of_fraction);
        // multiply by `den` by removing decimal point from string representation
        String nums = doubles.replace(".", "");
        Apint num = new Apint(nums);
        return new ABSRational(new Aprational(num, den));
    }

    public int toInt() {
        return value.intValue();
    }

    public double toDouble() {
        return value.doubleValue();
    }

    public Aprational toAprational() {
        return value;
    }

    @Override
    public String toString() {
        return value.toString();
    }

    public ABSInteger truncate() {
        return ABSInteger.fromBigInt(value.truncate());
    }

    public ABSInteger numerator() {
        return ABSInteger.fromBigInt(value.numerator());
    }

    public ABSInteger denominator() {
        return ABSInteger.fromBigInt(value.denominator());
    }

    public ABSFloat toFloat() {
        return ABSFloat.fromDouble(value.doubleValue());
    }

}
