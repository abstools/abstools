package abs.backend.java.observing;

import java.util.List;

public interface ClassView {
    String getName();

    List<String> getFieldNames();

    List<String> getMethodNames();
}
