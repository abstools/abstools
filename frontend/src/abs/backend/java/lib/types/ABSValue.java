package abs.backend.java.lib.types;

public interface ABSValue extends ABSType {
    ABSBool eq(ABSValue o);

    ABSBool notEq(ABSValue o);

    boolean isDataType();

    boolean isReference();

}
