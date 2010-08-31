package abs.backend.java.observing;

import java.util.List;

import abs.backend.java.lib.types.ABSValue;

public interface ObjectListener {

    void methodCalled(String method, List<ABSValue> args);
    void fieldRead(String field, ABSValue value);
    void fieldUpdated(String field, ABSValue oldValue, ABSValue newValue);
    
}
 