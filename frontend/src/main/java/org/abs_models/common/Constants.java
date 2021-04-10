/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.common;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class Constants {
    public static final String STDLIB_NAME = "ABS.StdLib";

    public static final String FLI_NAME = "ABS.FLI";
    public static final String META_NAME = "ABS.Meta";
    public static final String DC_NAME = "ABS.DC";
    public static final String SCHEDULE_NAME = "ABS.Scheduler";
    public static final String PL_NAME = "ABS.Productline";
    public static final String EXCEPTIONS_NAME = "ABS.StdLib.Exceptions";
    public static final String SMARTDEPLOY_NAME = "ABS.SmartDeploy";
    public static final String JUNIT_NAME = "ABS.JUnit";

    public static final List<String> BUILT_IN_LIBS =
        Collections.unmodifiableList(Arrays.asList(STDLIB_NAME,
                                                   FLI_NAME,
                                                   META_NAME,
                                                   DC_NAME,
                                                   SCHEDULE_NAME,
                                                   PL_NAME,
                                                   EXCEPTIONS_NAME,
                                                   SMARTDEPLOY_NAME,
                                                   JUNIT_NAME));

    public static final String FUNCTIONAL_BREAK_POINT_FUNCTION = "watch";
    public static boolean isFunctionalBreakPointFunctionName(String functionName) {
        return functionName.equals(STDLIB_NAME + "." + FUNCTIONAL_BREAK_POINT_FUNCTION) ||
                functionName.equals(STDLIB_NAME + "." + FUNCTIONAL_BREAK_POINT_FUNCTION + "Ex");
    }
}
