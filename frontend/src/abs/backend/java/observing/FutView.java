package abs.backend.java.observing;

import abs.backend.java.lib.types.ABSValue;

public interface FutView {
    TaskView getResolvingTask();
    boolean isResolved();
    ABSValue getValue();
    
    void registerFutObserver(FutObserver obs);
}
