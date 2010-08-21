package abs.backend.java.lib.expr;

import java.util.HashMap;
import java.util.Map;

import abs.backend.java.lib.types.ABSDataType;

public class PatternBinding {
	Map<String, ABSDataType> binding = new HashMap<String, ABSDataType>();
	
	public void addBinding(String varName, ABSDataType d) {
		binding.put(varName, d);
	}
}
