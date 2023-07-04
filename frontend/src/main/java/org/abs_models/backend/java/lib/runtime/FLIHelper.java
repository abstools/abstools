/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.lib.types.ABSString;

/**
 * useful helper methods which can be used by FLI clients  
 */
public class FLIHelper {
    
    public static void print(String msg) {
        ABSRuntime.getRuntime().getOutStream().print(msg);
    }
    
    public static void print(ABSString msg) {
        print(msg.getString());
    }
    
    public static void println(String msg) {
        print(msg + "\n");
    }
    
    public static void println(ABSString msg) {
        println(msg.getString());
    }
}
