/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

public class ABSDeadlockException extends ABSException {
    public ABSDeadlockException() {
        super("A Deadlock has been detected");
    }

    @Override
    public boolean isDeadlock() {
        return true;
    }

    @Override
    public String getName() {
        return "Deadlock";
    }
}
