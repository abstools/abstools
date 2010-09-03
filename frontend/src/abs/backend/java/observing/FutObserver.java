package abs.backend.java.observing;

import abs.backend.java.lib.types.ABSValue;

public interface FutObserver {
    void onResolved(FutView fut, ABSValue value);
}
