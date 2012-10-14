/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.sql;

import abs.common.Constants;
import abs.frontend.typechecker.Type;

public class Util {
    
    /**
     * @param type
     * @return the name for the given type in the ABS DB library, or null if the
     *         given type is not supported or unknown
     */
    public static String getAbsDbTypeName(final Type type) {
        if (type.isStringType())
            return "String";
        if (type.isIntType())
            return "Int";
        if (type.isBoolType())
            return "Bool";
        if (type.isNullType())
            return "Null";
        return null;
    }

    /**
     * @param type
     * @return for a given ABS type name, such as Int, returns the corresponding
     *         type name for the ABS DB library, or null if the type is not
     *         supported
     */
    public static String convertAbsToDbTypeName(final String type) {
        if ("String".equals(type) || "Int".equals(type)  || "Bool".equals(type))
            return type;
        if ((Constants.STDLIB_NAME + ".String").equals(type))
            return "String";
        if ((Constants.STDLIB_NAME + ".Int").equals(type))
            return "Int";
        if ((Constants.STDLIB_NAME + ".Bool").equals(type))
            return "Bool";
        return null;
    }

}
