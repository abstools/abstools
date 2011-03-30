/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import beaver.Symbol;

public class ABSSymbol extends Symbol {
    private final int abspos;
    public ABSSymbol(short id, int line, int column, int length, int abspos, Object text) {
        super(id, line, column, length, text);
        this.abspos = abspos;
    }
    
    public int getAbsolutePosition() {
        return abspos;
    }
}
