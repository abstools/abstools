package abs.frontend.parser;

//Note: this must be an unchecked exception,
//since otherwise we the Java stub class Event (defined in ABS.parser)
//cannot override method Parser.Events.scannerError
public class LexicalError extends RuntimeException {

	private static final long serialVersionUID = 1L;
	public int lineNumber = -1;
	public int columnNumber = -1;

	public LexicalError(final String message) {
		super(message);
	}

	public LexicalError(final String message, int lineNumber, int columnNumber) {
		super(message);
		this.lineNumber = lineNumber;
		this.columnNumber = columnNumber;
	}
}
