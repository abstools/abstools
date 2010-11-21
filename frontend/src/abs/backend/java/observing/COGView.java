package abs.backend.java.observing;

public interface COGView {
    TaskSchedulerView getScheduler();

    void registerObjectCreationListener(ObjectCreationObserver listener);

    void registerObjectCreationListener(String className, ObjectCreationObserver e);

    int getID();
}
