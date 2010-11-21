package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSValue;

public class AnyPattern extends Pattern {

    @Override
    public boolean match(ABSValue dt, PatternBinding binding) {
        return true;
    }

}
