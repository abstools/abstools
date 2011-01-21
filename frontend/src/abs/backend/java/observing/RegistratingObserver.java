package abs.backend.java.observing;

import java.util.List;

import abs.backend.java.lib.types.ABSValue;


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
