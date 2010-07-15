package abs.backend.java;

import abs.backend.java.lib.ABSAndGuard;
import abs.backend.java.lib.ABSExpGuard;
import abs.backend.java.lib.ABSFutureGuard;
import abs.backend.java.lib.ABSRuntime;
import abs.backend.java.lib.ABSType;

public class JavaBackendConstants {
    public static String LIB_PACKAGE = ABSType.class.getPackage().getName();
    public static String LIB_IMPORT_STATEMENT = "import "+LIB_PACKAGE+".*;";
    public static String ABSRUNTIME = ABSRuntime.class.getName();
    public static String ANDGUARD = ABSAndGuard.class.getName();
    public static String CLAIMGUARD = ABSFutureGuard.class.getName();
    public static String EXPGUARD = ABSExpGuard.class.getName();
    
    
}
