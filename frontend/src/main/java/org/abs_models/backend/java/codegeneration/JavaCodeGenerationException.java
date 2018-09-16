/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.codegeneration;

public class JavaCodeGenerationException extends Exception {
    private static final long serialVersionUID = 4444515759094137086L;

    public JavaCodeGenerationException(String message) {
        super(message);
    }

    public JavaCodeGenerationException(Throwable t) {
        super(t);
    }

    public JavaCodeGenerationException(String m, Throwable t) {
        super(m,t);
    }
}
