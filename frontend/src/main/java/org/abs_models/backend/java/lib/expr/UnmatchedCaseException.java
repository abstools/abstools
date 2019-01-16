/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.expr;

import org.abs_models.backend.java.lib.runtime.ABSException;

public class UnmatchedCaseException extends ABSException {
    private static final long serialVersionUID = 1L;

    public UnmatchedCaseException(String msg) {
        super(msg);
    }

    @Override
    public String getName() {
        return "Unmatched Case in Pattern Matching";
    }

}
