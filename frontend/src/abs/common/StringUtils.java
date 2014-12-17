/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import com.google.common.base.Joiner;

public class StringUtils {
    
    /** 
     * Capitalizes the first letter (and only the first!) of the string. 
     * 
     * If the string is empty, it returns the same string; otherwise,
     * returns a copy of the string with the first letter capitalized.
     * 
     * @param s The string to capitalize.
     * 
     * @author Andri Saar <andri@cs.ioc.ee>
     */
    public static final String capitalize(String s) {
        if (s == null || s.length() == 0) 
            return s;
        
        return Character.toUpperCase(s.charAt(0)) + s.substring(1);
    }
    
    /**
     * Joins the given array of names using a dot "."
     */
    public static final String joinNames(String... names) {
        Joiner joiner = Joiner.on(".").skipNulls();
        return joiner.join(names);
    }
}
