package abs.backend.java.lib.types;

public class ABSObject {
    public ABSBool eq(ABSRef o) {
        return ABSBool.fromBoolean(this == o);
    }
}
