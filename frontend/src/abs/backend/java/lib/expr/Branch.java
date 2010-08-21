package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSInteger;

public abstract class Branch<R> {
	Pattern pattern;
	public Branch(Pattern p) {
		this.pattern = p;
	}
	
	public Pattern getPattern() {
	   return pattern;
   }

	public R apply(PatternBinding binding) {
	   return null;
   }
	
}




class Test {
	/**
	 * data Foo = X | Bar(Foo);
	 * 
	 * Foo b = Bar(Bar(X));
	 * case b { X => 1; Bar(X) => 2; Bar(x)False => 2; }
	 */
	
	void m() {
		ABSBool b = ABSBool.TRUE;
		new Case<ABSBool, ABSInteger>() { }.of(b);
	}
}