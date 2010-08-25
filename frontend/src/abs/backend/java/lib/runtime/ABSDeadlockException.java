package abs.backend.java.lib.runtime;

public class ABSDeadlockException extends ABSException {
    public ABSDeadlockException() {
        super("A Deadlock has been detected");
    }
}
