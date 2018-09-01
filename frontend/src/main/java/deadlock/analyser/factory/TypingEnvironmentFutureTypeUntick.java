/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser.factory;

public class TypingEnvironmentFutureTypeUntick implements ITypingEnvironmentFutureType {

    IRecord r;
    ContractElementInvk c;
    
    public TypingEnvironmentFutureTypeUntick(IRecord r, ContractElementInvk c){
        this.r = r;
        this.c = c;
    }
    
    public IRecord getRecord(){return r;}
    public ContractElementInvk getContract(){return c;}
}
