/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.prettyprint;

import java.io.PrintWriter;

import org.abs_models.frontend.tests.ABSFormatter;

public class DefaultABSFormatter implements ABSFormatter {

    private final PrintWriter w;
    private static final String INDENT = "  ";
    private StringBuilder indentation = new StringBuilder();
    
    public DefaultABSFormatter(PrintWriter w) {
        this.w = w;
    }

    @Override
    public void beforeOpenBrace() {
    }

    @Override
    public void afterOpenBrace() {
        indentation.append(INDENT);
    }

    @Override
    public void afterStmt() {
        w.print(indentation.toString());
    }

    @Override
    public void beforeCloseBrace() {
        indentation.deleteCharAt(indentation.length()-1);
        indentation.deleteCharAt(indentation.length()-1);
    }

    @Override
    public void afterCloseBrace() {
    }

}
