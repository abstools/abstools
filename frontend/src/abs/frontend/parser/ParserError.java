package abs.frontend.parser;

import abs.common.CompilerError;

public class ParserError extends CompilerError {

	public ParserError(String msg, int line, int column) {
		super(msg, line, column);
	}

}
