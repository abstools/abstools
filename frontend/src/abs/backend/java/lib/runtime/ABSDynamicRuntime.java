/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import abs.backend.java.codegeneration.dynamic.DynamicException;

public class ABSDynamicRuntime extends ABSRuntime {

    /* 
     * Object roster maintains a list of objects in the system
     * 
     */
    private Map<ABSDynamicClass, Set<WeakReference<ABSDynamicObject>>> objectRoster 
        = new HashMap<ABSDynamicClass, Set<WeakReference<ABSDynamicObject>>>();
    
    public void registerObject(ABSDynamicObject obj) {
        Set<WeakReference<ABSDynamicObject>> objectSet;
        if (objectRoster.get(obj.getClazz()) == null) {
            objectSet = new HashSet<WeakReference<ABSDynamicObject>>();
            objectRoster.put(obj.getClazz(), objectSet);
        } else {
            objectSet = objectRoster.get(obj.getClazz());
        }
        objectSet.add(new WeakReference<ABSDynamicObject>(obj));
    }
    
    
    /*
     * Keep track of currently configured product of the SPL
     * (used for runtime-adaptable models)
     */
    private ABSDynamicProduct currentProduct = null;
    public ABSDynamicProduct getCurrentProduct() {
        return currentProduct;
    }
    public void setCurrentProduct(ABSDynamicProduct prod) {
        currentProduct = prod;
    }

}
