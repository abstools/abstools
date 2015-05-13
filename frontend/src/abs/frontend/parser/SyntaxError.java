/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import beaver.Symbol;

public class SyntaxError extends ParserError {

    private final Symbol token;

    public SyntaxError(final String message) {
        this(message, 0, 0, null);
    }

    public SyntaxError(final String message, int lineNumber, int columnNumber, Symbol token) {
        super(message, lineNumber, columnNumber);
        // Editor requires this info also for invalid input:
        assert token != null : message;
        this.token = token;
    }

    public int getEndLine() { return Symbol.getLine(token.getEnd()); };
    public int getEndColumn() { return Symbol.getColumn(token.getEnd()); };
}
