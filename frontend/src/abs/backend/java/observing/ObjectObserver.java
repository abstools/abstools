package abs.backend.java.observing;

import java.util.List;

import abs.backend.java.lib.types.ABSValue;

public interface ObjectObserver {

    void methodCalled(ObjectView object, String method, List<ABSValue> args);

    void fieldRead(ObjectView object, String field, ABSValue value);

    void fieldUpdated(ObjectView object, String field, ABSValue oldValue, ABSValue newValue);

}
