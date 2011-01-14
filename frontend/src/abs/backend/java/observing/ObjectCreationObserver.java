package abs.backend.java.observing;

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
