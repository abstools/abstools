/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

public interface ObjectCreationObserver {
    /**
     * Is called after an object has been created, but before
     * it has been initialized
     * @param o the object that was created
     */
    void objectCreated(ObjectView o);
    
    /**
     * Is called after an object has been fully initialized,
     * e.g., all fields have been initialized and the init block
     * has been invoked
     * @param o the object that has been initialized
     */
    void objectInitialized(ObjectView o);
}
