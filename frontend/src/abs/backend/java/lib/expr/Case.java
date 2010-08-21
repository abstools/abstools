package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;

public abstract class Case<V extends ABSDataType,R> {
	final Branch<R>[] branches;
	public Case(Branch<R>... b) {
		branches = b;
	}
	public R of(V v) {
		for (Branch<R> b : branches) {
			PatternBinding binding = v.match(b.getPattern());
			
			if (binding != null) {
				return b.apply(binding);
			}
		}
		return null;
	}

}
