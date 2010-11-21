package abs.backend.java.lib.runtime;

public class ABSAssertException extends ABSException {
    private static final long serialVersionUID = 1L;

    public ABSAssertException(String string) {
        super(string);
    }

    @Override
    public boolean isAssertion() {
        return true;
    }

    @Override
    public String getName() {
        return "Assertion Failed";
    }
}
