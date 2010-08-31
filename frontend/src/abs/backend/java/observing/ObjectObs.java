package abs.backend.java.observing;

import abs.backend.java.lib.types.ABSValue;

public interface ObjectObs {
    COGObs getCOG();
    ClassObs getClassObs();
    String getClassName();
    ABSValue getFieldValue(String fieldName);
    
    void registerObjectListener(ObjectListener l);
}
