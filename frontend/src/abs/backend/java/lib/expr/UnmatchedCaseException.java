package abs.backend.java.lib.expr;

public class UnmatchedCaseException extends RuntimeException {
    private static final long serialVersionUID = 1L;
    
    public UnmatchedCaseException(String msg) {
        super(msg);
    }

}
