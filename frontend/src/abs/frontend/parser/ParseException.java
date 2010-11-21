package abs.frontend.parser;

public class ParseException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    private final ParserError error;

    public ParseException(ParserError e) {
        super(e.getHelpMessage());
        error = e;
    }

    public ParserError getError() {
        return error;
    }
}
