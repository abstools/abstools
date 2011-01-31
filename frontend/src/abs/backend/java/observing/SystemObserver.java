package abs.backend.java.observing;

/**
 * An interface to observer global system behavior
 * 
 * @author Jan Schäfer
 *
 */
public interface SystemObserver {
    void systemStarted();

    void newCOGCreated(COGView cog, ObjectView initialObject);
    
    void systemFinished();
}
