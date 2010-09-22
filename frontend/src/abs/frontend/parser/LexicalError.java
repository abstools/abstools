package abs.frontend.parser;

public class LexicalError extends ParserError {
	public LexicalError(String msg, int line, int column) {
		super(msg, line, column);
	}
}
