package abs.backend.java.observing;

import java.util.List;

import abs.backend.java.lib.types.ABSValue;

public interface TaskObs {
    ObjectObs getTarget();
    COGObs getCOG();
    String getMethodName();
    List<ABSValue> getArgs();
    FutObs getFuture();
}
