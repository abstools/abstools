package abs.backend.java.lib.runtime;

public class ABSNullPointerException extends ABSException {
    private static final long serialVersionUID = 1L;

    public ABSNullPointerException() {
        super("An access to null has been detected");
    }

    public boolean isNullPointer() {
        return true;
    }

    @Override
    public String getName() {
        return "Null Access";
    }

}
