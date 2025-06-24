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
import org.abs_models.backend.java.lib.types.ABSInterface;
import org.abs_models.backend.java.lib.types.ABSProcess;
import org.abs_models.backend.java.lib.types.ABSUnit;
import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.backend.java.utils.DynamicClassUtils;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.apfloat.AprationalMath;

public class ABSBuiltInFunctions {

    static long ms_at_model_start = System.currentTimeMillis();

    private static final PrintStream out = new PrintStream(System.out, true, StandardCharsets.UTF_8);

    public static Apint strlen(String s) {
        return new Apint(s.length());
    }

    public static String substr(String s, Apint from, Apint length) {
        return s.substring(from.intValue(), from.intValue() + length.intValue());
    }

    public static ABSUnit print(String s) {
        out.print(s);
        return ABSUnit.UNIT;
    }

    public static Aprational currentms() {
        return ABSRuntime.getRuntime().getClock();
    }

    public static Apint ms_since_model_start() {
        return new Apint(System.currentTimeMillis() - ms_at_model_start);
    }

    public static Aprational lowlevelDeadline() {
        Aprational deadline_t = ABSThread.getCurrentTask().getDeadlineAbsolute();
        if (deadline_t.signum() >= 0) {
            Aprational clock = ABSRuntime.getRuntime().getClock();
            Aprational deadline_r = deadline_t.subtract(clock);
            // We clamp the deadline at 0, since lowlevelDeadline() < 0 means no deadline given.
            return AprationalMath.max(deadline_r, Aprational.ZERO);
        } else {
            return new Aprational(-1);
        }
    }

    public static Apint random(Apint i) {
        if (BinOp.ltEq(i, Apint.ZERO)) {
            throw new UnmatchedCaseException("Random function called with non positive upper bound " + i);
        }
        // TODO: use ApintMath.random(digits)
        BigInteger n = i.toBigInteger();
        Random rand = ABSRuntime.getRuntime().getRandom();

        BigInteger result = new BigInteger(n.bitLength(), rand);
        while (result.compareTo(n) >= 0) {
            result = new BigInteger(n.bitLength(), rand);
        }
        return new Apint(result);
    }

    public static ABSInterface thisDC() {
        return ABSThread.getCurrentCOG().getDC();
    }

    public static <T> String toString(T t) {
        return switch (t) {
            case null -> "null";
            case Boolean b when b -> "True";
            case Boolean b when !b -> "False";
            default -> t.toString();
        };
    }

    /*
     * functions related to user-defined schedulers (see abslang, module
     * ABS.Scheduler)
     */
    public static String method(ABSProcess p) {
        return p.getMethodName();
    }

    public static Object arrival(ABSProcess p) {
        Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Time_Time");
        return DynamicClassUtils.instance(type, new Aprational(p.getArrivalTime()));
    }

    public static Object cost(ABSProcess p) {
        if (p.getCost().signum() == -1) {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_InfDuration");
            return DynamicClassUtils.instance(type);
        } else {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_Duration");
            return DynamicClassUtils.instance(type, p.getCost());
        }
    }

    public static Object proc_deadline(ABSProcess p) {
        if (p.getDeadlineAbsolute().signum() == -1) {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_InfDuration");
            return DynamicClassUtils.instance(type);
        } else {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_Duration");
            return DynamicClassUtils.instance(type, p.getDeadlineAbsolute().subtract(ABSRuntime.getRuntime().getClock()));
        }
    }

    public static Object start(ABSProcess p) {
        Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Time_Time");
        return DynamicClassUtils.instance(type, new Aprational(p.getStartTime()));
    }

    public static Object finish(ABSProcess p) {
        Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Time_Time");
        return DynamicClassUtils.instance(type, new Aprational(p.getFinishTime()));
    }

    public static boolean critical(ABSProcess p) {
        return p.isCritical();
    }

    public static Apint value(ABSProcess p) {
        return new Apint(p.getValue());
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

    public static Aprational min(Aprational r1, Aprational r2) {
        return r1.compareTo(r2) < 0 ? r1 : r2;
    }

    public static Apint min(Apint i1, Apint i2) {
        return i1.compareTo(i2) < 0 ? i1 : i2;
    }

    public static Double min(Double d1, Double d2) {
        return d1 < d2 ? d1 : d2;
    }

    public static Aprational max(Aprational r1, Aprational r2) {
        return r1.compareTo(r2) > 0 ? r1 : r2;
    }

    public static Apint max(Apint i1, Apint i2) {
        return i1.compareTo(i2) > 0 ? i1 : i2;
    }

    public static Double max(Double d1, Double d2) {
        return d1 > d2 ? d1 : d2;
    }

    public static Aprational abs(Aprational r) {
        return r.compareTo(Aprational.ZERO) < 0 ? r.negate() : r;
    }

    public static Apint abs(Apint i) {
        return i.compareTo(Apint.ZERO) < 0 ? i.negate() : i;
    }

    public static Double abs(Double d) {
        return Math.abs(d);
    }

    public static Apint truncate(Aprational r) {
        return r.truncate();
    }

    public static Apint numerator(Aprational r) {
        return r.numerator();
    }

    public static Apint denominator(Aprational r) {
        return r.denominator();
    }

    public static double float__(Aprational r) {
        return r.doubleValue();
    }

    public static Aprational rat(double f) {
        return new Aprational(f);
    }

    public static Apint floor(double f) {
        return new Apint(Math.round(Math.floor(f)));
    }

    public static Apint ceil(double f) {
        return new Apint(Math.round(Math.ceil(f)));
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
