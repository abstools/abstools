package abs.frontend.parser;

import abs.common.CompilerError;

public class ParserError extends CompilerError {
    private final int line;
    private final int column;
    private final String message;

    public ParserError(String msg, int line, int column) {
        this.message = msg;
        this.line = line;
        this.column = column;
    }

    @Override
    public String getMessage() {
        return message;
    }

    /**
     * @return the line number of the error, or -1 if unavailable.
     */
    @Override
    public int getLine() {
        return line;
    }

    /**
     * @return the column number of the error, or -1 if unavailable.
     */
    @Override
    public int getColumn() {
        return column;
    }

}
