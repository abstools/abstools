/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser.factory;

public class TypingEnvironmentFutureTypeTick implements ITypingEnvironmentFutureType {

    IRecord r;
        
    public TypingEnvironmentFutureTypeTick(IRecord r){
        this.r = r;
        
    }
    
    public IRecord getRecord(){return r;}
    
    
}
