package abs.backend.prolog;

public class PrologBackendException extends RuntimeException {
	public PrologBackendException(String msg) {
		super("Prolog Backend exception: " + msg);
	}
}
