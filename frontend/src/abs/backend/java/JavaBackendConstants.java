package abs.backend.java;

import abs.backend.java.lib.ABSType;

public class JavaBackendConstants {
    public static String LIB_PACKAGE = ABSType.class.getPackage().getName();
    public static String LIB_IMPORT_STATEMENT = "import "+LIB_PACKAGE+".*;";
}
