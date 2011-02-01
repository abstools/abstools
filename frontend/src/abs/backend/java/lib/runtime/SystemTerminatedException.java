package abs.backend.java.lib.runtime;

public class SystemTerminatedException extends RuntimeException {
    private static final long serialVersionUID = 1L;
    
    public SystemTerminatedException() {
        super("The ABS Runtime has been terminated");
    }

}
