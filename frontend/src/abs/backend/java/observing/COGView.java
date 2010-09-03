package abs.backend.java.observing;

public interface COGView {
    SchedulerView getScheduler();
    
    void registerObjectCreationListener(ObjectCreationObserver listener);
    void registerObjectCreationListener(String className, ObjectCreationObserver e);
}
