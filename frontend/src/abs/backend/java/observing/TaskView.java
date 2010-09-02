package abs.backend.java.observing;

import java.util.List;

import abs.backend.java.lib.types.ABSValue;

public interface TaskView {
    ObjectView getTarget();
    COGView getCOG();
    String getMethodName();
    List<ABSValue> getArgs();
    FutView getFuture();
    
    void registerTaskListener(TaskListener listener);
}
