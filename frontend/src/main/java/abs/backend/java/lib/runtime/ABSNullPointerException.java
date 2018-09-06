/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

public class ABSNullPointerException extends ABSException {
    private static final long serialVersionUID = 1L;

    public ABSNullPointerException() {
        super("An access to null has been detected");
    }

    public boolean isNullPointer() {
        return true;
    }

    @Override
    public String getName() {
        return "Null Access";
    }

}
