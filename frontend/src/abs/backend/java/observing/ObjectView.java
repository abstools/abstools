package abs.backend.java.observing;

import java.util.List;

import abs.backend.java.lib.types.ABSValue;

public interface ObjectView {
    COGView getCOG();

    ClassView getClassView();

    String getClassName();

    ABSValue getFieldValue(String fieldName) throws NoSuchFieldException;

    void registerObjectObserver(ObjectObserver l);

    List<String> getFieldNames();
}
