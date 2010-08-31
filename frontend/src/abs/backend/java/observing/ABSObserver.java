package abs.backend.java.observing;

public abstract class ABSObserver {
    private ABSSystem system;
    public final void setABSEvents(ABSSystem e) {
        system = e;
    }
    
    public ABSSystem getSystem() {
        return system;
    }
    
    

}
