/**
 * Copyright (c) 2014, Rudolf Schlatte. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.antlr.parser;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

import abs.frontend.parser.ParseException;
import abs.frontend.parser.ParserError;
import abs.frontend.parser.SyntaxError;

public class SyntaxErrorCollector extends BaseErrorListener {

    private java.io.File filename = null;
    boolean raiseExceptions = false;

    public SyntaxErrorCollector(java.io.File file, boolean raiseExceptions) {
        this.filename = file;
        this.raiseExceptions = raiseExceptions;
    }

    public void setRaiseExceptions(boolean raiseExceptions) {
        this.raiseExceptions = raiseExceptions;
    }

    public java.util.List<ParserError> parserErrors
        = new java.util.ArrayList<ParserError>();

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                            int line, int charPositionInLine, String msg,
                            RecognitionException e)
    {
        SyntaxError err = new SyntaxError(msg, line, charPositionInLine + 1);
        err.setFile(filename);
        parserErrors.add(err);
        // super.syntaxError(recognizer, offendingSymbol, line, charPositionInLine, msg, e);
        if (raiseExceptions) {
            throw new ParseException(err);
        }
    }
}
