package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;

public abstract class Case<V extends ABSDataType,R> {
	public abstract R of(V v);

}
