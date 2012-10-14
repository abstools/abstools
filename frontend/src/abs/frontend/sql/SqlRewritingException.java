/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.sql;

import abs.frontend.analyser.SemanticError;

/**
 * This class is used within the parser to pass through errors during rewriting
 * SQL expressions to core ABS code.
 * 
 * @author Marko Martin
 */
public class SqlRewritingException extends Exception {

    private static final long serialVersionUID = 1L;
    
    private final SemanticError error;

    public SqlRewritingException(SemanticError arg0, Throwable arg1) {
        super(arg1);
        this.error = arg0;
    }

    public SqlRewritingException(SemanticError arg0) {
        this.error = arg0;
    }

    public SemanticError getError() {
        return error;
    }

}
