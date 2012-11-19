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

import abs.backend.java.lib.expr.UnmatchedCaseException;
import abs.backend.java.lib.runtime.metaABS.ObjectMirrorClass;
import abs.backend.java.lib.runtime.metaABS.RuntimeClass;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.backend.java.lib.types.ABSValue;

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

    public static ABSInteger currentms() {
        return ABSInteger.fromLong(System.currentTimeMillis());
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
        while( result.compareTo(n) >= 0 ) {
            result = new BigInteger(n.bitLength(), rand);
        }
        return ABSInteger.fromBigInt(result);
    }
    
    public static <T> ABSString toString(T t) {
        return ABSString.fromString(t.toString());
    }
    
    /* reflect creates a "mirror object" for the given object, 
     * which gives access to the meta API.
     */
    public static <T> ABSDynamicObject reflect(T t) {
        String name = "$mirror";
        try {
            ABSValue existingMirror = ((ABSDynamicObject)t).getFieldValue(name);
            return (ABSDynamicObject)existingMirror;
        } catch(NoSuchFieldException e) {
            ABSDynamicObject mirror = new ABSDynamicObject(ObjectMirrorClass.singleton());
            mirror.setFieldValue("object", (ABSValue)t);
            ((ABSDynamicObject)t).setFieldValue(name, (ABSValue)mirror);
            return mirror;
        }
    }

    public static ABSDynamicObject getRuntime() {
        ABSDynamicObject runtime = new ABSDynamicObject(RuntimeClass.singleton());
        return runtime;
    }
    

    /* Convenience functions, to be removed 
     * 
     */
    public static ABSUnit println(ABSString s) {
        try {
            PrintStream out = new PrintStream(System.out, true, "UTF-8");
            out.println(s.getString());
        } catch (UnsupportedEncodingException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        //System.out.println(s.getString());
        return ABSUnit.UNIT;
    }

    public static ABSString readln() {
        Scanner scanner = new Scanner(System.in);
        String line = scanner.nextLine();
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
    
}
