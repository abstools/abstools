/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

public abstract class ABSException extends RuntimeException {

    private long randomSeed = -1;
    
    public ABSException(String string) {
        super(string);
    }

    public ABSException(String string, long randomSeed) {
        super(string);
        this.randomSeed = randomSeed;
    }

    @Override
    public String getMessage() {
        String res = super.getMessage();
        if (randomSeed != -1)
            res += "\n Random seed = "+randomSeed;
        
        return res;
    }

    public boolean isDeadlock() {
        return false;
    }

    public boolean isAssertion() {
        return false;
    }

    public boolean isIllegalSynchronousCall() {
        return false;
    }

    public boolean isNullPointer() {
        return false;
    }

    public abstract String getName();

}
