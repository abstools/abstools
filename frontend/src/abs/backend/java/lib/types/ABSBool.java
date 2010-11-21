package abs.backend.java.lib.types;

public class ABSBool extends ABSBuiltInDataType {
    public static ABSBool TRUE = new ABSBool("True", true);
    public static ABSBool FALSE = new ABSBool("False", false);

    private boolean value;

    private ABSBool(String constr, boolean v) {
        super(constr);
        this.value = v;
    }

    @Override
    public ABSBool eq(ABSValue o) {
        return ABSBool.fromBoolean(o == this);
    }

    @Override
    public ABSBool notEq(ABSValue o) {
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
