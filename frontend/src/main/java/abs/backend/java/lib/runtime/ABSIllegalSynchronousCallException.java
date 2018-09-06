/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

public class ABSIllegalSynchronousCallException extends ABSException {

    public ABSIllegalSynchronousCallException() {
        super("A synchronous call targeting an object of a different COG has been detected.");
    }

    @Override
    public boolean isIllegalSynchronousCall() {
        return true;
    }

    @Override
    public String getName() {
        return "Illegal Synchronous Call";
    }
}
