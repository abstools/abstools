/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

public class ABSMainClass extends ABSDynamicClass {
    private static ABSMainClass instance;
    
    public static ABSMainClass singleton() {
        if (instance == null) instance = new ABSMainClass();
        return instance;
    }
    
    private ABSMainClass () {
        setName("Main");
    }

}
