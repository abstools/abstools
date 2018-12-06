/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.codegeneration.dynamic;

import org.abs_models.backend.java.lib.runtime.ABSException;

public class DynamicException extends ABSException {

    public DynamicException(String string) {
        super(string);
    }

    private static final long serialVersionUID = -8269533628421261068L;

    @Override
    public String getName() {
        return "DynamicException";
    }

}
