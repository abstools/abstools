/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.common;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Collectors;

public class StringUtils {

    /**
     * Capitalizes the first letter (and only the first!) of the string.
     *
     * If the string is empty, it returns the same string; otherwise,
     * returns a copy of the string with the first letter capitalized.
     *
     * @param s The string to capitalize.
     */
    public static final String capitalize(String s) {
        if (s == null || s.length() == 0)
            return s;

        return Character.toUpperCase(s.charAt(0)) + s.substring(1);
    }

    /**
     * Join the given array of names using an arbitrary string j
     */
    public static final String join(String j, String... names) {
        return join(j, Arrays.asList(names));
    }

    /**
     * Join the given String Collection using an arbitrary string j
     */
    public static String join(String j, Collection<String> strings) {
        return strings.stream()
            .filter(s -> s != null)
            .collect(Collectors.joining(j));
    }

}
