/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

public class Constants {
    public static final String STDLIB_NAME = "ABS.StdLib";

    /**
     * whether to automatically include the DB library (located in
     * src/abs/lang/db) during parsing a model<br>
     * This is discouraged because it makes parsing slower. When using a model
     * based on the DB extensions (e.g., if it uses the SQL language extensions
     * - see {@link abs.frontend.sql.SqlTransformationTests}), you must then - if this constant
     * is false - do one of the following options:
     * <p>
     * 1. Use the ABS tools with the option "-dblib" which will include the ABS
     * database files.
     * <br>
     * OR
     * <br>
     * 2. Copy the database model files into your model project manually.
     */
    public static final boolean USE_DBLIB_BY_DEFAULT = false;
    public static final String DBLIB_NAME = "ABS.DB";
    public static final String DB_STRUCTURE_LIB_NAME = DBLIB_NAME + ".Structure";
    public static final String DB_OPERATORS_LIB_NAME = DBLIB_NAME + ".Operators";
    public static final String DB_OPERATORS_STRUCTURE_LIB_NAME = DB_OPERATORS_LIB_NAME + ".Structure";
    public static final String DB_TRANSACTIONS_LIB_NAME = DBLIB_NAME + ".Transactions";
    public static final String DB_HELPERS_LIB_NAME = DBLIB_NAME + ".Helpers";
    
    public static final String FUNCTIONAL_BREAK_POINT_FUNCTION = "watch";
    public static boolean isFunctionalBreakPointFunctionName(String functionName) {
        return functionName.equals(STDLIB_NAME + "." + FUNCTIONAL_BREAK_POINT_FUNCTION) ||
                functionName.equals(STDLIB_NAME + "." + FUNCTIONAL_BREAK_POINT_FUNCTION + "Ex");
    }
}
