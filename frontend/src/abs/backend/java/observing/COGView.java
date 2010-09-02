package abs.backend.java.observing;

public interface COGView {
    SchedulerView getScheduler();
    
    void registerObjectCreationListener(ObjectCreationListener listener);
    void registerObjectCreationListener(String className, ObjectCreationListener e);
}
