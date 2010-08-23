package abs.frontend.typechecker;

public class BoundedType extends Type {
	private Type boundType;
	
	public void bindTo(Type t) {
		boundType = t;
	}
	
	public Type getBoundType() {
		return boundType;
	}
	
	@Override
	public boolean isSubtypeOf(Type t) {
		if (hasBoundType())
			return boundType.isSubtypeOf(t);
		boundType = t;
	   return true;
	}
	
	@Override
	public boolean isBoundedType() {
		return true;
	}
	
	public boolean hasBoundType() {
		return boundType != null;
	}
	
	@Override
   public String toString() {
		if (hasBoundType()) {
			return boundType.toString();
		}
	   return "Unbound Type";
   }
	

}
