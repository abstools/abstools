package abs.backend.java.observing;

public interface SystemObserver {
    void systemStarted();
    void newCOGCreated(COGView cog, ObjectView initialObject);
}
