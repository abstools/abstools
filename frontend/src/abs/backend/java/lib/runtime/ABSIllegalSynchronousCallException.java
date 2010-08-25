package abs.backend.java.lib.runtime;

public class ABSIllegalSynchronousCallException extends ABSException {

    public ABSIllegalSynchronousCallException() {
        super("A synchronous call targeting an object of a different COG has been detected.");
    }

}
