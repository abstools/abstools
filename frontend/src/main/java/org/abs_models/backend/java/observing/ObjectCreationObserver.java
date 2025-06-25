/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

public interface ObjectCreationObserver {
    /**
     * This method is called after an object has been created, but
     * before it has been initialized.
     *
     * <p>Note that this method is not called for the first object of
     * a cog; instead, the first object is passed as the second
     * parameter of {@link SystemObserver#newCOGCreated}.
     *
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
