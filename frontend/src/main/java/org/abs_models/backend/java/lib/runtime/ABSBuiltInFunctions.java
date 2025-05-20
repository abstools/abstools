/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.io.PrintStream;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Random;
import java.util.Scanner;

import org.abs_models.backend.java.lib.expr.BinOp;
import org.abs_models.backend.java.lib.expr.UnmatchedCaseException;
import org.abs_models.backend.java.lib.runtime.metaABS.ObjectMirror;
import org.abs_models.backend.java.lib.runtime.metaABS.ProductLine;
import org.abs_models.backend.java.lib.types.ABSDataType;
import org.abs_models.backend.java.lib.types.ABSInteger;
import org.abs_models.backend.java.lib.types.ABSInterface;
import org.abs_models.backend.java.lib.types.ABSProcess;
import org.abs_models.backend.java.lib.types.ABSRational;
import org.abs_models.backend.java.lib.types.ABSUnit;
import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.backend.java.utils.DynamicClassUtils;
import org.apfloat.Aprational;
import org.apfloat.AprationalMath;

public class ABSBuiltInFunctions {

    static long ms_at_model_start = System.currentTimeMillis();

    private static final PrintStream out = new PrintStream(System.out, true, StandardCharsets.UTF_8);

    public static ABSInteger strlen(String s) {
        return ABSInteger.fromInt(s.length());
    }

    public static String substr(String s, ABSInteger from, ABSInteger length) {
        return s.substring(from.toInt(), from.toInt() + length.toInt());
    }

    public static ABSUnit print(String s) {
        out.print(s);
        return ABSUnit.UNIT;
    }

    public static ABSRational currentms() {
        return ABSRational.fromAprational(ABSRuntime.getRuntime().getClock());
    }

    public static ABSInteger ms_since_model_start() {
        return ABSInteger.fromLong(System.currentTimeMillis() - ms_at_model_start);
    }

    public static ABSRational lowlevelDeadline() {
        Aprational deadline_t = ABSThread.getCurrentTask().getDeadlineAbsolute();
        if (deadline_t.signum() >= 0) {
            Aprational clock = ABSRuntime.getRuntime().getClock();
            Aprational deadline_r = deadline_t.subtract(clock);
            // We clamp the deadline at 0, since lowlevelDeadline() < 0 means no deadline given.
            return ABSRational.fromAprational(AprationalMath.max(deadline_r, Aprational.ZERO));
        } else {
            return ABSRational.fromLong(-1);
        }
    }

    public static ABSInteger random(ABSInteger i) {
        if (BinOp.ltEq(i, ABSInteger.ZERO)) {
            throw new UnmatchedCaseException("Random function called with non positive upper bound " + i);
        }
        BigInteger n = i.getBigInteger();
        Random rand = ABSRuntime.getRuntime().getRandom();

        BigInteger result = new BigInteger(n.bitLength(), rand);
        while (result.compareTo(n) >= 0) {
            result = new BigInteger(n.bitLength(), rand);
        }
        return ABSInteger.fromBigInt(result);
    }

    public static ABSInterface thisDC() {
        return ABSThread.getCurrentCOG().getDC();
    }

    public static <T> String toString(T t) {
        if (t == null) {
            return "null";
        } else {
            return t.toString();
        }
    }

    /*
     * functions related to user-defined schedulers (see abslang, module
     * ABS.Scheduler)
     */
    public static String method(ABSProcess p) {
        return p.getMethodName();
    }

    public static ABSDataType arrival(ABSProcess p) {
        Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Time_Time");
        return DynamicClassUtils.instance(type, ABSRational.fromLong(p.getArrivalTime()));
    }

    public static ABSDataType cost(ABSProcess p) {
        if (p.getCost().signum() == -1) {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_InfDuration");
            return DynamicClassUtils.instance(type);
        } else {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_Duration");
            return DynamicClassUtils.instance(type, ABSRational.fromAprational(p.getCost()));
        }
    }

    public static ABSDataType proc_deadline(ABSProcess p) {
        if (p.getDeadlineAbsolute().signum() == -1) {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_InfDuration");
            return DynamicClassUtils.instance(type);
        } else {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_Duration");
            return DynamicClassUtils.instance(type, ABSRational.fromAprational(p.getDeadlineAbsolute()
                                                                               .subtract(ABSRuntime.getRuntime().getClock())));
        }
    }

    public static ABSDataType start(ABSProcess p) {
        Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Time_Time");
        return DynamicClassUtils.instance(type, ABSRational.fromLong(p.getStartTime()));
    }

    public static ABSDataType finish(ABSProcess p) {
        Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Time_Time");
        return DynamicClassUtils.instance(type, ABSRational.fromLong(p.getFinishTime()));
    }

    public static boolean critical(ABSProcess p) {
        return p.isCritical();
    }

    public static ABSInteger value(ABSProcess p) {
        return ABSInteger.fromInt(p.getValue());
    }

    /*
     * Functions to access the meta level
     * 
     * reflect creates a "mirror object" for the given object, which gives
     * access to the meta API.
     */
    public static <T> ABSDynamicObject reflect(T t) {
        String name = "$mirror";
        try {
            Object existingMirror = ((ABSDynamicObject) t).getFieldValue(name);
            return (ABSDynamicObject) existingMirror;
        } catch (NoSuchFieldException e) {
            ABSDynamicObject mirror = new ABSDynamicObject(ObjectMirror.singleton());
            mirror.setFieldValue("object", t);
            ((ABSDynamicObject) t).setFieldValue(name, mirror);
            return mirror;
        }
    }

    public static ABSDynamicObject getProductLine() {
        ABSDynamicObject pl = new ABSDynamicObject(ProductLine.singleton());
        return pl;
    }

    /*
     * Convenience functions, to be removed
     */
    public static ABSUnit println(String s) {
        out.println(s);
        return ABSUnit.UNIT;
    }

    public static String readln() {
        try (Scanner scanner = new Scanner(System.in)) {
	        String line = scanner.nextLine();
	        return line.trim();
	    }
    }

    /*
     * Functional break point
     * 
     * Keep the value of the following constant in sync with the name of the
     * method and the according built-in function defined in abslang.abs.
     */
    public static <A extends ABSValue, B extends ABSValue> A watchEx(String fileName, int line, A val, B info) {
        ABSRuntime runtime = ABSRuntime.getRuntime();
        if (runtime.debuggingEnabled()) {
            Task<?> task = ABSThread.getCurrentTask();
            task.newStackFrame(null, "$watch");
            task.setLocalVariable("$watchValue", val);
            task.setLocalVariable("$watchInfo", info);
            runtime.nextStep(fileName, line);
            task.popStackFrame();
        }
        return val;
    }

    public static <A extends ABSValue> A watch(String fileName, int line, A val) {
        ABSRuntime runtime = ABSRuntime.getRuntime();
        if (runtime.debuggingEnabled()) {
            Task<?> task = ABSThread.getCurrentTask();
            task.newStackFrame(null, "$watch");
            task.setLocalVariable("$watchValue", val);
            runtime.nextStep(fileName, line);
            task.popStackFrame();
        }
        return val;
    }

    public static ABSRational min(ABSRational r1, ABSRational r2) {
        return r1.toAprational().compareTo(r2.toAprational()) < 0 ? r1 : r2;
    }

    public static ABSInteger min(ABSInteger i1, ABSInteger i2) {
        return i1.toAprational().compareTo(i2.toAprational()) < 0 ? i1 : i2;
    }

    public static Double min(Double d1, Double d2) {
        return d1 < d2 ? d1 : d2;
    }

    public static ABSRational max(ABSRational r1, ABSRational r2) {
        return r1.toAprational().compareTo(r2.toAprational()) > 0 ? r1 : r2;
    }

    public static ABSInteger max(ABSInteger i1, ABSInteger i2) {
        return i1.toAprational().compareTo(i2.toAprational()) > 0 ? i1 : i2;
    }

    public static Double max(Double d1, Double d2) {
        return d1 > d2 ? d1 : d2;
    }

    public static ABSRational abs(ABSRational r) {
        return r.toAprational().compareTo(Aprational.ZERO) < 0
            ? ABSRational.fromAprational(r.toAprational().negate())
            : r;
    }

    public static ABSInteger abs(ABSInteger i) {
        return i.toAprational().compareTo(Aprational.ZERO) < 0
            ? ABSInteger.fromBigInt(i.toAprational().negate())
            : i;
    }

    public static Double abs(Double d) {
        return Math.abs(d);
    }

    public static ABSInteger truncate(ABSRational r) {
        return r.truncate();
    }

    public static ABSInteger numerator(ABSRational r) {
        return r.numerator();
    }

    public static ABSInteger denominator(ABSRational r) {
        return r.denominator();
    }

    public static double float__(ABSRational r) {
        return r.toDouble();
    }

    public static ABSRational rat(double f) {
        return ABSRational.fromDouble(f);
    }

    public static ABSInteger floor(double f) {
        return ABSInteger.floor(f);
    }

    public static ABSInteger ceil(double f) {
        return ABSInteger.ceil(f);
    }

    public static double sqrt(double f) {
        return StrictMath.sqrt(f);
    }

    public static double log(double f) {
        return StrictMath.log(f);
    }

    public static double exp(double f) {
        return StrictMath.exp(f);
    }
}
