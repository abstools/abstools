package abs.backend.java;

import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.types.ABSAndGuard;
import abs.backend.java.lib.types.ABSExpGuard;
import abs.backend.java.lib.types.ABSFutureGuard;
import abs.backend.java.lib.types.ABSType;

public class JavaBackendConstants {
    public static String LIB_PACKAGE = ABSType.class.getPackage().getName();
    public static String LIB_IMPORT_STATEMENT = "import "+LIB_PACKAGE+".*; import "+LIB_PACKAGE+".expr.*;";
    public static String ABSRUNTIME = ABSRuntime.class.getName();
    public static String ANDGUARD = ABSAndGuard.class.getName();
    public static String CLAIMGUARD = ABSFutureGuard.class.getName();
    public static String EXPGUARD = ABSExpGuard.class.getName();
    
    
}
