/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import org.abs_models.backend.java.lib.runtime.ABSAndGuard;
import org.abs_models.backend.java.lib.runtime.ABSDurationGuard;
import org.abs_models.backend.java.lib.runtime.ABSExpGuard;
import org.abs_models.backend.java.lib.runtime.ABSFutureGuard;
import org.abs_models.backend.java.lib.runtime.ABSRuntime;
import org.abs_models.backend.java.lib.types.ABSUnit;

public class JavaBackendConstants {
    //public static String LIB_TYPES_PACKAGE = ABSType.class.getPackage().getName();
    //public static String LIB_EXPR_PACKAGE = Let.class.getPackage().getName();
    //public static String LIB_IMPORT_STATEMENT = "import " + LIB_TYPES_PACKAGE + ".*; " + "import " + LIB_EXPR_PACKAGE
    //        + ".*;";
    public final static String UNITVALUE = ABSUnit.class.getName()+".UNIT";
    public final static String ABSRUNTIME = ABSRuntime.class.getName();
    public final static String ANDGUARD = ABSAndGuard.class.getName();
    public final static String CLAIMGUARD = ABSFutureGuard.class.getName();
    public final static String EXPGUARD = ABSExpGuard.class.getName();
    public final static String DURATIONGUARD = ABSDurationGuard.class.getName();

    // Package names for generated products, deltas
    public final static String LIB_RDM_PACKAGE = "rdm";
    public final static String LIB_DELTAS_PACKAGE = "deltas";
    public final static String LIB_UPDATES_PACKAGE = "updates";
}
