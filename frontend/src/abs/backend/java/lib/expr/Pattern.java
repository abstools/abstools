package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSValue;

public abstract class Pattern {
    public PatternBinding match(ABSValue dt) {
        PatternBinding b = new PatternBinding();
        if (match(dt, b))
            return b;
        else
            return null;
    }

    public abstract boolean match(ABSValue dt, PatternBinding binding);

}
