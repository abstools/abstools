package abs.backend.java.observing;

import abs.backend.java.lib.types.ABSValue;

public interface FutObs {
    TaskObs getResolvingTask();
    boolean isResolved();
    ABSValue getValue();
}
