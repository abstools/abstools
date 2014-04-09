/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser.detection;

import java.util.HashMap;
import java.util.Map;

import deadlock.analyser.factory.Factory;
import deadlock.constraints.term.Term;

public abstract class DASolver {

    
    protected Boolean deadlock;
    protected Map<String, Term> methodMap;
    protected Factory df;
    protected Map<String, BigLam> lampMap;
    
    public DASolver(Factory f, Map<String, Term> map){
        this.df = f;
        this.methodMap = map;
                
        this.deadlock = false;
        
        this.lampMap = new HashMap<String, BigLam>();

        for(String mName : methodMap.keySet()){
            this.lampMap.put(mName, new BigLam(mName, methodMap.get(mName)));
        }
    }
    
    public boolean isDeadlockMain(){
        return this.deadlock;
    }
    
    public abstract void computeSolution();
}
