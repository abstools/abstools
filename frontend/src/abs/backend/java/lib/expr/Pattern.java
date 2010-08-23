package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;

public abstract class Pattern {
	public PatternBinding match(ABSDataType dt) {
	    PatternBinding b = new PatternBinding();
	    if (match(dt,b))
	        return b;
	    else
	        return null;
	}

    public abstract boolean match(ABSDataType dt, PatternBinding binding);

}
