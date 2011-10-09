/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;

public class ABSBuiltInFunctions {
    public static ABSInteger strlen(ABSString s) {
        return s.strlen();
    }

    public static ABSString substr(ABSString s, ABSInteger from, ABSInteger length) {
        return s.substr(from, length);
    }

    public static ABSUnit print(ABSString s) {
        System.out.println(s.getString());
        return ABSUnit.UNIT;
    }

    public static ABSInteger currentms() {
        return ABSInteger.fromLong(System.currentTimeMillis());
    }
    
    public static ABSInteger lowlevelDeadline() {
        return ABSInteger.fromInt(-1);
    }
    
    public static ABSInteger random(ABSInteger i) {
        ABSRuntime rt = ABSRuntime.getCurrentRuntime();
        /* [stolz] Several issues:
         * - there's no builtin nextLong(range)
         * - if we'd use nextLong(), we'd need to spin until finding a value in range.
         * So let's do with just ints for now.
         */
        int rnd = rt.getRandom().nextInt(i.toInt()); // FIXME: i can be a BigInteger!
        return ABSInteger.fromInt(rnd);
    }
}
