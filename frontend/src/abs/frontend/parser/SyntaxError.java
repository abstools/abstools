package abs.frontend.parser;

import abs.common.CompilerError;

public class SyntaxError extends ParserError  {

	private static final long serialVersionUID = 1L;

	public SyntaxError(final String message) {
		this(message,0,0);
	}

	public SyntaxError(final String message, int lineNumber, int columnNumber) {
		super(message,lineNumber,columnNumber);
	}

}
