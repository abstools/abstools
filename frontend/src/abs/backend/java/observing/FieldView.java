package abs.backend.java.observing;

import abs.backend.java.lib.types.ABSType;

public interface FieldView {
    public ClassView getClassView();
    public String getName();
    public ABSType getType();
}
