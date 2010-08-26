package abs.backend.java.lib.expr;

import java.util.ArrayList;

import abs.backend.java.lib.types.ABSValue;

public class PatternBinding {
	ArrayList<ABSValue> binding = new ArrayList<ABSValue>();
	
	public void addBinding(ABSValue dt) {
	    binding.add(dt);
	}
	
	public ABSValue getBinding(int i) {
	    return binding.get(i);
	}
}
