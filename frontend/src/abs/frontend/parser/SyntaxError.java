package abs.frontend.parser;

// Note: this must be an unchecked exception,
// since otherwise we the Java stub class Event (defined in ABS.parser)
// cannot override method Parser.Events.syntaxError
public class SyntaxError extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public SyntaxError(final String message) {
		super(message);
	}

}
