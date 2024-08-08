/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.types;

import java.math.BigInteger;

import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;

/**
 * Implementation of an ABS integer
 * 
 * @author Jan Schäfer
 * 
 */
public class ABSInteger extends ABSRational {
    public static final ABSInteger ZERO = new ABSInteger(Apint.ZERO);
    public static final ABSInteger ONE = new ABSInteger(Apint.ONE);

    private ABSInteger(Aprational i) {
        super();
        this.value = i;
    }

    public ABSInteger add(ABSInteger i) {
        return fromBigInt(this.value.add(i.value));
    }

    public ABSInteger subtract(ABSInteger i) {
        return fromBigInt(this.value.subtract(i.value));
    }

    public ABSInteger multiply(ABSInteger i) {
        return fromBigInt(this.value.multiply(i.value));
    }

    public ABSRational divide(ABSInteger i) {
        return ABSRational.fromAprational(this.value).divide(i);
    }

    public ABSInteger mod(ABSInteger i) {
        return fromBigInt(this.value.mod(i.value));
    }

    public ABSInteger negate() {
        return fromBigInt(this.value.negate());
    }

    public static ABSInteger fromBigInt(BigInteger i) {
        return new ABSInteger(new Aprational(i));
    }

    public static ABSInteger fromBigInt(Aprational i) {
        return new ABSInteger(i);
    }

    public static ABSInteger fromString(String value) {
        return fromBigInt(new Apint(value));
    }

    public static ABSInteger fromInt(int i) {
        switch (i) {
        case 0:
            return ZERO;
        case 1:
            return ONE;
        default:
            return fromBigInt(new Apint(i));
        }
    }

    public static ABSInteger fromLong(long l) {
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

    public BigInteger getBigInteger() {
        return value.truncate().toBigInteger();
    }

    @Override
    public ABSInteger truncate() {
        return this;
    }

    @Override
    public ABSInteger numerator() {
        return this;
    }

    @Override
    public ABSInteger denominator() {
        return ONE;
    }

    public static ABSInteger floor(ABSFloat f) {
        return new ABSInteger(new Apfloat(f.getDouble()).floor());
    }

    public static ABSInteger ceil(ABSFloat f) {
        return new ABSInteger(new Apfloat(f.getDouble()).ceil());
    }

}
