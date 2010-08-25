package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;

public class ABSBuiltInFunctions {
    public static ABSInteger strlen(ABSString s) {
        return s.strlen();
    }

    public static ABSString substr(ABSString s, ABSInteger from, ABSInteger length) {
        return s.substr(from,length);
    }

}
