/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.util.Random;
import java.util.Scanner;

import org.apfloat.Apint;
import org.apfloat.Aprational;

import abs.backend.java.lib.expr.UnmatchedCaseException;
import abs.backend.java.lib.runtime.metaABS.ObjectMirror;
import abs.backend.java.lib.runtime.metaABS.ProductLine;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSDataType;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSProcess;
import abs.backend.java.lib.types.ABSRational;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.utils.DynamicClassUtils;

public class ABSBuiltInFunctions {

    public static ABSInteger strlen(ABSString s) {
        return s.strlen();
    }

    public static ABSString substr(ABSString s, ABSInteger from, ABSInteger length) {
        return s.substr(from, length);
    }

    public static ABSUnit print(ABSString s) {
        System.out.print(s.getString());
        return ABSUnit.UNIT;
    }

    public static ABSRational currentms() {
        return ABSRational.fromBigInt(new Aprational(new Apint(System.currentTimeMillis()), new Apint(1000)));
    }

    public static ABSInteger lowlevelDeadline() {
        return ABSInteger.fromInt(-1);
    }

    public static ABSInteger random(ABSInteger i) {
        if (i.ltEq(ABSInteger.ZERO).toBoolean()) {
            throw new UnmatchedCaseException("Random function called with non positive upper bound " + i);
        }
        BigInteger n = i.getBigInteger();
        Random rand = ABSRuntime.getCurrentRuntime().getRandom();

        BigInteger result = new BigInteger(n.bitLength(), rand);
        while (result.compareTo(n) >= 0) {
            result = new BigInteger(n.bitLength(), rand);
        }
        return ABSInteger.fromBigInt(result);
    }

    public static abs.backend.java.lib.types.ABSInterface thisDC() {
        return ABSRuntime.getCurrentCOG().getDC();
    }

    public static <T> ABSString toString(T t) {
        if (t == null) {
            return ABSString.fromString("null");
        } else {
            return ABSString.fromString(t.toString());
        }
    }

    /*
     * functions related to user-defined schedulers (see abslang, module
     * ABS.Scheduler)
     */
    public static ABSString method(ABSProcess p) {
        return ABSString.fromString(p.getMethodName());
    }

    public static ABSDataType arrival(ABSProcess p) {
        Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Time_Time");
        return DynamicClassUtils.instance(type, ABSRational.fromLong(p.getArrivalTime()));
    }

    public static ABSDataType cost(ABSProcess p) {
        if (p.getCost() == -1) {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_InfDuration");
            return DynamicClassUtils.instance(type);
        } else {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_Duration");
            return DynamicClassUtils.instance(type, ABSRational.fromLong(p.getCost()));
        }
    }

    public static ABSDataType procDeadline(ABSProcess p) {
        if (p.getDeadline() == -1) {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_InfDuration");
            return DynamicClassUtils.instance(type);
        } else {
            Class<?> type = DynamicClassUtils.getClass("ABS.StdLib.Duration_Duration");
            return DynamicClassUtils.instance(type, ABSRational.fromLong(p.getDeadline()));
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

    public static ABSBool critical(ABSProcess p) {
        return ABSBool.fromBoolean(p.isCritical());
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
            ABSValue existingMirror = ((ABSDynamicObject) t).getFieldValue(name);
            return (ABSDynamicObject) existingMirror;
        } catch (NoSuchFieldException e) {
            ABSDynamicObject mirror = new ABSDynamicObject(ObjectMirror.singleton());
            mirror.setFieldValue("object", (ABSValue) t);
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
    public static ABSUnit println(ABSString s) {
        try {
            PrintStream out = new PrintStream(System.out, true, "UTF-8");
            out.println(s.getString());
        } catch (UnsupportedEncodingException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        // System.out.println(s.getString());
        return ABSUnit.UNIT;
    }

    public static ABSString readln() {
        Scanner scanner = new Scanner(System.in);
        String line = scanner.nextLine();
        //scanner.close();
        return ABSString.fromString(line.trim());
    }

    /*
     * Functional break point
     * 
     * Keep the value of the following constant in sync with the name of the
     * method and the according built-in function defined in abslang.abs.
     */
    public static <A extends ABSValue, B extends ABSValue> A watchEx(String fileName, int line, A val, B info) {
        ABSRuntime runtime = ABSRuntime.getCurrentRuntime();
        if (runtime.debuggingEnabled()) {
            Task<?> task = ABSRuntime.getCurrentTask();
            task.newStackFrame(null, "$watch");
            task.setLocalVariable("$watchValue", val);
            task.setLocalVariable("$watchInfo", info);
            runtime.nextStep(fileName, line);
            task.popStackFrame();
        }
        return val;
    }

    public static <A extends ABSValue> A watch(String fileName, int line, A val) {
        ABSRuntime runtime = ABSRuntime.getCurrentRuntime();
        if (runtime.debuggingEnabled()) {
            Task<?> task = ABSRuntime.getCurrentTask();
            task.newStackFrame(null, "$watch");
            task.setLocalVariable("$watchValue", val);
            runtime.nextStep(fileName, line);
            task.popStackFrame();
        }
        return val;
    }

    public static ABSInteger truncate(ABSRational r) {
        return r.truncate();
    }

}
