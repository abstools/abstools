/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.parser;

import org.abs_models.common.CompilerCondition;

public class ParserError extends CompilerCondition {
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
    public int getStartLine() {
        return line;
    }

    /**
     * @return the column number of the error, or -1 if unavailable.
     */
    @Override
    public int getStartColumn() {
        return column;
    }

    @Override
    public boolean isError() {
        return true;
    }

    @Override
    public boolean isWarning() {
        return false;
    }
}
