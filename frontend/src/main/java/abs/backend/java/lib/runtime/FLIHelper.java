/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSString;

/**
 * useful helper methods which can be used by FLI clients  
 */
public class FLIHelper {
    
    public static void print(String msg) {
        ABSRuntime.getCurrentRuntime().getOutStream().print(msg);
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
