/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.utils;

public class StringUtil {

    public static String iterableToString(Iterable<?> it, String sep) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (Object o : it) {
            if (first)
                first = false;
            else
                sb.append(sep);
            sb.append(""+o);

        }
        return sb.toString();
    }

    public static <A, B extends ToString<A>> String iterableToString(Iterable<A> it, String sep, B toString) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (A a : it) {
            if (first)
                first = false;
            else
                sb.append(sep);
            sb.append(toString.toString(a));

        }
        return sb.toString();
    }

}
