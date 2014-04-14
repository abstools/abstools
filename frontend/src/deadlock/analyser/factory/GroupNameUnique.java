/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser.factory;

public class GroupNameUnique extends GroupName {
    private GroupNameUnique(){
        super(null);
    }
    
    private static GroupNameUnique instance;
    
    public static GroupNameUnique GetInstance()
    {
        return instance == null? instance = new GroupNameUnique() : instance;
    }
    
    @Override
    public String toString()
    {
        return "$";
    }
    
    @Override
    public boolean equals(Object other)
    {
        return other == this;
    }
    
    @Override
    public int hashCode()
    {
        return "$".hashCode();
    }
}
