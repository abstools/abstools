package abs.frontend.typechecker;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.List;
import abs.frontend.ast.MethodSig;

public class UnionType extends ReferenceType {
	private final java.util.List<InterfaceType> types;
	
	public UnionType(List<InterfaceTypeUse> types) {
		this.types = new ArrayList<InterfaceType>();
		for (InterfaceTypeUse t : types) {
			this.types.add((InterfaceType)t.getType());
		}
	}

	public UnionType(InterfaceType... types) {
		this.types = new ArrayList<InterfaceType>();
		this.types.addAll(Arrays.asList(types));
	}
	
	public java.util.List<InterfaceType> getTypes() {
		return Collections.unmodifiableList(types);
	}
	
	public InterfaceType getType(int i) {
	    return types.get(i);
	}
	
	
	@Override
	public boolean equals(Object o) {
		if (!super.equals(o))
			return false;
		UnionType t = (UnionType) o;
		return t.types.equals(this.types);
	}
	
	@Override
	public int hashCode() {
	   return types.hashCode();
	}
	
	@Override
	public boolean isSubtypeOf(Type t) {
	    for (InterfaceType it : types) {
	        if (it.isSubtypeOf(t))
	            return true;
	    }
	    return super.isSubtypeOf(t);
	}
	
	@Override
	public MethodSig lookupMethod(String name) {
	    for (InterfaceType t : types) {
	        MethodSig s = t.lookupMethod(name);
	        if (s != null)
	            return s;
	    }
	    return null;
	}
}
