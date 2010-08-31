package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;

public class ABSBuiltInFunctions {
    public static ABSInteger strlen(ABSString s) {
        return s.strlen();
    }

    public static ABSString substr(ABSString s, ABSInteger from, ABSInteger length) {
        return s.substr(from,length);
    }

    public static ABSUnit print(ABSString s) {
        System.out.println(s.getString());
        return ABSUnit.UNIT;
    }
    
    public static ABSInteger currentms() {
        return ABSInteger.fromLong(System.currentTimeMillis());
    }
}
