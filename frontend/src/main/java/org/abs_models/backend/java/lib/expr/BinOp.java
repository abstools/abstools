/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.expr;

import org.abs_models.backend.java.lib.runtime.ABSDynamicClass;
import org.abs_models.backend.java.lib.runtime.ABSFut;
import org.abs_models.backend.java.lib.runtime.ABSObject;
import org.abs_models.backend.java.lib.runtime.COG;
import org.abs_models.backend.java.lib.types.ABSAlgebraicDataType;
import org.abs_models.backend.java.lib.types.ABSBool;
import org.abs_models.backend.java.lib.types.ABSFloat;
import org.abs_models.backend.java.lib.types.ABSProcess;
import org.abs_models.backend.java.lib.types.ABSRational;
import org.abs_models.backend.java.lib.types.ABSString;
import org.abs_models.backend.java.lib.types.ABSUnit;
import org.abs_models.backend.java.lib.types.ABSValue;

/**
 * Comparisons between ABS values.
 */
public class BinOp {

    public static ABSBool eq(ABSValue v1, ABSValue v2) {
        // Various branches in the following switch statement rely on the
        // typechecker to only allow through type-correct programs, which
        // means that v1 and v2 have the same ABS type.
        switch (v1) {
            case null: return ABSBool.fromBoolean(v2 == null);
            case ABSDynamicClass c1:
                if (v2 instanceof ABSDynamicClass c2) {
                    return ABSBool.fromBoolean(c1.getName().equals(c2.getName()));
                }
                break;
            case COG c: return ABSBool.fromBoolean(c == v2);
            case ABSProcess p: return ABSBool.fromBoolean(p == v2);
            case ABSObject o: return ABSBool.fromBoolean(o == v2);
            case ABSFut<?> f: return ABSBool.fromBoolean(f == v2);
            case ABSUnit u: return ABSBool.fromBoolean(v2 instanceof ABSUnit);
            case ABSBool b1: if (v2 instanceof ABSBool b2) {
                    return ABSBool.fromBoolean(b1.toBoolean() == b2.toBoolean());
                }
                break;
            case ABSRational r1: if (v2 instanceof ABSRational r2) { // includes ABSInteger
                    return ABSBool.fromBoolean(r1.toAprational().compareTo(r2.toAprational()) == 0);
                }
                break;
            case ABSFloat f1: if (v2 instanceof ABSFloat f2) {
                    return ABSBool.fromBoolean(f1.getDouble() == f2.getDouble());
                }
                break;
            case ABSString s1: if (v2 instanceof ABSString s2) {
                    return ABSBool.fromBoolean(s1.getString().equals(s2.getString()));
                }
                break;
            case ABSAlgebraicDataType t1:
                if (v2 instanceof ABSAlgebraicDataType t2) {
                    if (!t1.getConstructorName().equals(t2.getConstructorName())) return ABSBool.FALSE;
                    for (int i = 0; i < t1.getNumArgs(); i++) {
                        if (!BinOp.eq(t1.getArg(i), t2.getArg(i)).toBoolean())
                            return ABSBool.FALSE;
                    }
                    return ABSBool.TRUE;
                }
                break;
            default:
                throw new RuntimeException("Encountered unknown ABS type " + v1.getClass() + " in BinOp#eq, giving up.");
        }
        // not reached except in case of mistyped program
        return ABSBool.FALSE;
    }

    public static ABSBool notEq(ABSValue v, ABSValue v2) {
        return eq(v, v2).negate();
    }

    public static ABSBool gt(ABSValue v1, ABSValue v2) {
        switch (v1) {
            case null: return ABSBool.FALSE;
            case ABSDynamicClass c1:
                if (v2 instanceof ABSDynamicClass c2) {
                    return ABSBool.fromBoolean(c1.getName().compareTo(c2.getName()) > 0);
                }
                break;
            case COG c1:
                if (v2 instanceof COG c2) {
                    return ABSBool.fromBoolean(c1.getID() > c2.getID());
                } else if (v2 == null) {
                    return ABSBool.TRUE;
                }
                break;
            case ABSProcess p1:
                if (v2 instanceof ABSProcess p2) {
                    return ABSBool.fromBoolean(p1.getPid() > p2.getPid());
                }
                break;
            case ABSObject o1:
                if (v2 instanceof ABSObject o2) {
                    int comp = o1.getClassName().compareTo(o2.getClassName());
                    if (comp == 0) return ABSBool.fromBoolean(o1.getView().getID() > o2.getView().getID());
                    else return ABSBool.fromBoolean(comp > 0);
                } else if (v2 == null) {
                    return ABSBool.TRUE;
                }
                break;
            case ABSFut<?> f1:
                if (v2 instanceof ABSFut f2) {
                    return ABSBool.fromBoolean(f1.getID() > f2.getID());
                }
                break;
            case ABSUnit u: return ABSBool.FALSE;
            case ABSBool b1:
                if (v2 instanceof ABSBool b2) {
                    // "True" > "False"
                    return b1.and(b2.negate());
                }
                break;
            case ABSRational r1:
                if (v2 instanceof ABSRational r2) { // includes ABSInteger
                    return ABSBool.fromBoolean(r1.toAprational().compareTo(r2.toAprational()) > 0);
                }
                break;
            case ABSFloat f1:
                if (v2 instanceof ABSFloat f2) {
                    return ABSBool.fromBoolean(f1.getDouble() > f2.getDouble());
                }
                break;
            case ABSString s1:
                if (v2 instanceof ABSString s2) {
                    return ABSBool.fromBoolean(s1.getString().compareTo(s2.getString()) > 0);
                }
                break;
            case ABSAlgebraicDataType t1:
                if (v2 instanceof ABSAlgebraicDataType t2) {
                    int constructorComparison = t1.getConstructorName().compareTo(t2.getConstructorName());
                    if (constructorComparison == 0) {
                        for (int i = 0; i < t1.getNumArgs(); i++) {
                            if (BinOp.gt(t1.getArg(i), t2.getArg(i)).equals(ABSBool.TRUE))
                                return ABSBool.TRUE;
                        }
                        return ABSBool.FALSE;
                    } else {
                        return ABSBool.fromBoolean(constructorComparison > 0);
                    }
                }
                break;
            default:
                throw new RuntimeException("Encountered unknown ABS type " + v1.getClass() + " in BinOp#gt, giving up.");
        }
        // not reached except in case of mistyped program
        return ABSBool.FALSE;
    }

    // Note: the below methods could be open-coded to make one pass over v1,
    // v2 instead of two -- we go for simpler code instead of performance for
    // now.

    public static ABSBool lt(ABSValue v1, ABSValue v2) {
        ABSBool eq = eq(v1, v2);
        if (eq.toBoolean()) return ABSBool.FALSE;
        else return gt(v1, v2).negate();
    }

    public static ABSBool gtEq(ABSValue v1, ABSValue v2) {
        ABSBool eq = eq(v1, v2);
        if (eq.toBoolean()) return ABSBool.TRUE;
        else return gt(v1, v2);
    }

    public static ABSBool ltEq(ABSValue v1, ABSValue v2) {
        ABSBool eq = eq(v1, v2);
        if (eq.toBoolean()) return ABSBool.TRUE;
        else return gt(v1, v2).negate();
    }

}
