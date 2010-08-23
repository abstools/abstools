package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;

public class PatternValue extends Pattern {

    private final ABSDataType value;
    public PatternValue(ABSDataType value) {
        this.value = value;
    }
    
    @Override
    public boolean match(ABSDataType dt, PatternBinding binding) {
        return value.eq(dt).toBoolean();
    }
    
}
