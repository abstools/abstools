/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

public class LexicalError extends ParserError {
    public LexicalError(String msg, int line, int column) {
        super(msg, line, column);
    }
}
