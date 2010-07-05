package abs.backend.java.lib;

public class ABSBool implements ABSDataType {
    public static ABSBool TRUE = new ABSBool(true);
    public static ABSBool FALSE = new ABSBool(false);
    
    private boolean value;
    private ABSBool(boolean v) {
        this.value = v;
    }
    
    public ABSBool eq(ABSBool o) {
        return ABSBool.fromBoolean(o == this);
    }
    
    public ABSBool notEq(ABSBool o) {
        return ABSBool.fromBoolean(o != this);
    }
    
    public ABSBool negate() {
        return fromBoolean(!value);
    }

    public ABSBool and(ABSBool b) {
        return fromBoolean(value && b.value);
    }
    
    public ABSBool or(ABSBool b) {
        return fromBoolean(value || b.value);
    }
    
    public boolean toBoolean() {
        return value;
    }
    
    public static ABSBool fromBoolean(boolean b) {
        if (b)
            return TRUE;
        else
            return FALSE;
    }
}
