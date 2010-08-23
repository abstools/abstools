package abs.backend.java.lib.types;


public class ABSString extends ABSBuiltInDataType {
    public static final ABSString EMPTY = new ABSString("");
    
    private final String value;
    
    private ABSString(String s) {
        super("");
        this.value = s;
    }
    
    public ABSString add(ABSString s) {
        return fromString(value + s.value);
    }
    
    public ABSBool eq(ABSDataType o) {
        if (!super.eq(o).toBoolean())
            return ABSBool.FALSE;
        ABSString s = (ABSString) o;
        return ABSBool.fromBoolean(this.value.equals(s.value));
    }

    public static ABSString fromString(String s) {
        if (s.isEmpty())
            return EMPTY;
        return new ABSString(s);
    }

    public String getString() {
   	 return value;
    }
}
