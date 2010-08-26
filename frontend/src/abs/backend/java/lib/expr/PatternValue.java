package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSValue;

public class PatternValue extends Pattern {

    private final ABSValue value;
    public PatternValue(ABSValue value) {
        this.value = value;
    }
    
    @Override
    public boolean match(ABSValue dt, PatternBinding binding) {
        return value.eq(dt).toBoolean();
    }
    
}
