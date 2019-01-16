/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

public abstract class ABSException extends RuntimeException {

    private static final long serialVersionUID = 4203599845970796101L;
    private long randomSeed = -1;
    
    public ABSException(String string) {
        super(string);
    }

    public ABSException(String string, long randomSeed) {
        super(string);
        this.randomSeed = randomSeed;
    }

    @Override
    public String getMessage() {
        String res = super.getMessage();
        if (randomSeed != -1)
            res += "\n Random seed = "+randomSeed;
        
        return res;
    }

    public boolean isDeadlock() {
        return false;
    }

    public boolean isAssertion() {
        return false;
    }

    public boolean isIllegalSynchronousCall() {
        return false;
    }

    public boolean isNullPointer() {
        return false;
    }

    public abstract String getName();
    
    /**
     * gets the message and a stack trace as string
     * the stack trace is the java stack trace but without the elements from the backend 
     */
    public String getMessageWithStackTrace() {
        String result = getMessage();
        StackTraceElement[] trace = getStackTrace();
        for (StackTraceElement te : trace) {
           String className = te.getClassName();
           if (className.startsWith("org.abs_models.backend.java")) {
               break; // does not belong to the abs module
           }
           String methodName = te.getMethodName();
           className = className.replaceFirst("_(c|i)$", "");
           result += "\n  at " + className + "." + methodName;
        }
        return result;
        
    }

}
