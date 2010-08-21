package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;

public abstract class Pattern {
	public abstract boolean matches(ABSDataType dt);
}
