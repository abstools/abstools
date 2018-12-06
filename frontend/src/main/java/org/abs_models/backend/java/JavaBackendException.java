/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import org.abs_models.frontend.ast.ASTNode;

@SuppressWarnings("serial")
public class JavaBackendException extends RuntimeException {
    public JavaBackendException(String msg) {
        super("An exception in the Java backend of ABS occurred: " + msg);
    }

    public JavaBackendException(ASTNode<?> node, String msg) {
        super("An exception in the Java backend of ABS occurred: " + node.getStartLine() + ":"
                + node.getStartColumn() + ": " + msg);
    }

}
