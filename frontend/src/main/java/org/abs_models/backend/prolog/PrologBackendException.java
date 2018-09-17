/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.prolog;

public class PrologBackendException extends RuntimeException {
    public PrologBackendException(String msg) {
        super("Prolog Backend exception: " + msg);
    }
}
