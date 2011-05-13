package abs.fli.java;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;

public class PrimitiveUtil {
    public Integer convert(ABSInteger i) {
        return i.getNumArgs();
    }
    
    public ABSInteger convert(Integer i) {
        return ABSInteger.fromInt(i);
    }
    
    public String convert(ABSString s) {
        return s.getString();
    }
    
    public ABSString convert(String s) {
        return ABSString.fromString(s);
    }
    
    public Boolean convert(ABSBool b) {
        return b.toBoolean();
    }
    
    public ABSBool convert(Boolean b) {
        return ABSBool.fromBoolean(b);
    }
}
