/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala;

public class InitExpMetadata {
    private boolean hasInitExp = false;
    private boolean initExpSuspends = false;
    
    public InitExpMetadata() {
        
    }
    
    public InitExpMetadata(boolean hasInitExp, boolean initExpSuspends) {
        this.hasInitExp = hasInitExp;
        this.initExpSuspends = initExpSuspends;
    }
    
    public boolean hasInitExp() {
        return hasInitExp;
    }
    
    public boolean initExpSuspends() {
        return initExpSuspends;
    }
}
