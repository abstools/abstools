package abs.backend.java.lib.expr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import abs.backend.java.lib.types.ABSDataType;

public class PatternBinding {
	ArrayList<ABSDataType> binding = new ArrayList<ABSDataType>();
	
	public void addBinding(ABSDataType d) {
	    binding.add(d);
	}
	
	public ABSDataType getBinding(int i) {
	    return binding.get(i);
	}
}
