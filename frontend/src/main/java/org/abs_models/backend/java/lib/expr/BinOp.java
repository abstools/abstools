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
import org.abs_models.backend.java.lib.types.ABSProcess;
import org.abs_models.backend.java.lib.types.ABSUnit;
import org.apfloat.Aprational;

/**
 * Comparisons between ABS values.
 */
public class BinOp {

    public static boolean eq(Object v1, Object v2) {
        // Various branches in the following switch statement rely on the
        // typechecker to only allow through type-correct programs, which
        // means that v1 and v2 have the same ABS type.
        switch (v1) {
            case null: return v2 == null;
            case ABSDynamicClass c1:
                if (v2 instanceof ABSDynamicClass c2) {
                    return c1.getName().equals(c2.getName());
                }
                break;
            case COG c: return c == v2;
            case ABSProcess p: return p == v2;
            case ABSObject o: return o == v2;
            case ABSFut<?> f: return f == v2;
            case ABSUnit u: return v2 instanceof ABSUnit;
            case Boolean b1: return b1.equals(v2);
            case Aprational r1:
                if (v2 instanceof Aprational r2) { // includes Apint
                    return r1.compareTo(r2) == 0;
                }
                break;
            case Double f1:
                if (v2 instanceof Double f2) {
                    return f1.compareTo(f2) == 0;
                }
                break;
            case String s1:
                if (v2 instanceof String s2) {
                    return s1.equals(s2);
                }
                break;
            case ABSAlgebraicDataType t1:
                if (v2 instanceof ABSAlgebraicDataType t2) {
                    if (!t1.getConstructorName().equals(t2.getConstructorName())) return false;
                    for (int i = 0; i < t1.getNumArgs(); i++) {
                        if (!BinOp.eq(t1.getArg(i), t2.getArg(i)))
                            return false;
                    }
                    return true;
                }
                break;
            default:
                throw new RuntimeException("Encountered unknown ABS type " + v1.getClass() + " in BinOp#eq, giving up.");
        }
        // not reached except in case of mistyped program
        throw new RuntimeException("Encountered unknown ABS type " + v1.getClass() + " in BinOp#eq, giving up.");
    }

    public static boolean notEq(Object v, Object v2) {
        return !eq(v, v2);
    }

    public static boolean gt(Object v1, Object v2) {
        switch (v1) {
            case null: return false;
            case ABSDynamicClass c1:
                if (v2 instanceof ABSDynamicClass c2) {
                    return c1.getName().compareTo(c2.getName()) > 0;
                }
                break;
            case COG c1:
                if (v2 instanceof COG c2) {
                    return c1.getID() > c2.getID();
                } else if (v2 == null) {
                    return true;
                }
                break;
            case ABSProcess p1:
                if (v2 instanceof ABSProcess p2) {
                    return p1.getPid() > p2.getPid();
                }
                break;
            case ABSObject o1:
                if (v2 instanceof ABSObject o2) {
                    int comp = o1.getClassName().compareTo(o2.getClassName());
                    if (comp == 0) return o1.getView().getID() > o2.getView().getID();
                    else return comp > 0;
                } else if (v2 == null) {
                    return true;
                }
                break;
            case ABSFut<?> f1:
                if (v2 instanceof ABSFut f2) {
                    return f1.getID() > f2.getID();
                }
                break;
            case ABSUnit u: return false;
            case Boolean b1:
                if (v2 instanceof Boolean b2) {
                    // "True" > "False"
                    return b1 && !b2;
                }
                break;
            case Aprational r1:
                if (v2 instanceof Aprational r2) { // includes Apint
                    return r1.compareTo(r2) > 0;
                }
                break;
            case Double f1:
                if (v2 instanceof Double f2) {
                    return f1 > f2;
                }
                break;
            case String s1:
                if (v2 instanceof String s2) {
                    return s1.compareTo(s2) > 0;
                }
                break;
            case ABSAlgebraicDataType t1:
                if (v2 instanceof ABSAlgebraicDataType t2) {
                    int constructorComparison = t1.getConstructorName().compareTo(t2.getConstructorName());
                    if (constructorComparison == 0) {
                        for (int i = 0; i < t1.getNumArgs(); i++) {
                            if (BinOp.gt(t1.getArg(i), t2.getArg(i)))
                                return true;
                        }
                        return false;
                    } else {
                        return constructorComparison > 0;
                    }
                }
                break;
            default:
                throw new RuntimeException("Encountered unknown ABS type " + v1.getClass() + " in BinOp#gt, giving up.");
        }
        // not reached except in case of mistyped program
        throw new RuntimeException("Encountered unknown ABS type " + v1.getClass() + " in BinOp#gt, giving up.");
    }

    // Note: the below methods could be open-coded to make one pass over v1,
    // v2 instead of two -- we go for simpler code instead of performance for
    // now.

    public static boolean lt(Object v1, Object v2) {
        boolean eq = eq(v1, v2);
        if (eq) return false;
        else return !gt(v1, v2);
    }

    public static boolean gtEq(Object v1, Object v2) {
        boolean eq = eq(v1, v2);
        if (eq) return true;
        else return gt(v1, v2);
    }

    public static boolean ltEq(Object v1, Object v2) {
        boolean eq = eq(v1, v2);
        if (eq) return true;
        else return !gt(v1, v2);
    }

}
