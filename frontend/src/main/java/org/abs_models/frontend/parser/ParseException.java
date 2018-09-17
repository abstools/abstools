/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.parser;

public class ParseException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    private final ParserError error;

    public ParseException(ParserError e) {
        super(e.getHelpMessage());
        error = e;
    }

    public ParseException(String message) {
        super(message);
        error = null;
    }

    @SuppressWarnings("unused")
    private ParserError getError() {
        return error;
    }
}
