package abs.backend.java.observing;

import abs.backend.java.lib.types.ABSValue;

public interface FutObserver {
    void isResolved(FutView fut, ABSValue value);
}
