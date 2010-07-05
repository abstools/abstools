package abs.backend.java.lib;

public class ABSString implements ABSDataType {
    public static final ABSString EMPTY = new ABSString("");
    
    private final String value;
    
    private ABSString(String s) {
        this.value = s;
    }
    
    
    public ABSBool eq(ABSString s) {
        if (s == null)
            return ABSBool.FALSE;
        return ABSBool.fromBoolean(this.value.equals(s.value));
    }

    public ABSBool notEq(ABSString s) {
        return eq(s).negate();
    }
    
    public static ABSString fromString(String s) {
        if (s.isEmpty())
            return EMPTY;
        return new ABSString(s);
    }

}
