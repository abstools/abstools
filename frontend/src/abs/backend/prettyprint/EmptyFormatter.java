/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.prettyprint;

import java.io.PrintWriter;


public class EmptyFormatter implements ABSFormatter {

    @Override
    public void setPrintWriter(PrintWriter w) {
    }

    @Override
    public void beforeOpenBrace() {
    }

    @Override
    public void afterOpenBrace() {
    }

    @Override
    public void afterStmt() {

    }

    @Override
    public void beforeCloseBrace() {
    }

    @Override
    public void afterCloseBrace() {
    }

}
