package abs.backend.java.observing;

import java.util.List;

public interface ClassObs {
    String getName();
    List<String> getFieldNames();
    List<String> getMethodNames();
}
