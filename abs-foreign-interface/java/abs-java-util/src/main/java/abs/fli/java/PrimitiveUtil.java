package abs.fli.java;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;

public class PrimitiveUtil {
    public Integer convert(ABSInteger i) {
        return i.toInt();
    }
    
    public ABSInteger convert(Integer i) {
        return ABSInteger.fromInt(i);
    }
    
}
