/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

/**
 * A convenience class to realize CompleteObservers.
 * Does all the needed registrations to get all observation events.
 * Note that for the two methods newCOGCreated and objectCreated you
 * have to call the super methods when overriding these methods.
 * 
 * @author Jan Schäfer
 *
 */
public abstract class RegistratingObserver extends EmptyCompleteObserver {
    
    @Override
    public void newCOGCreated(COGView cog, ObjectView initialObject) {
        cog.registerObjectCreationListener(this);
        cog.getScheduler().registerTaskSchedulerObserver(this);
        initialObject.registerObjectObserver(this);
    }

    @Override
    public void objectCreated(ObjectView o) {
        o.registerObjectObserver(this);
    }

}
