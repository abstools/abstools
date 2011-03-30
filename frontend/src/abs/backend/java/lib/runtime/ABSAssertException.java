/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

public class ABSAssertException extends ABSException {
    private static final long serialVersionUID = 1L;

    public ABSAssertException(String string) {
        super(string);
    }

    @Override
    public boolean isAssertion() {
        return true;
    }

    @Override
    public String getName() {
        return "Assertion Failed";
    }
}
