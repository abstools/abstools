package abs.frontend.typechecker;

import abs.frontend.ast.MethodSig;

public abstract class Type {
    public String toString() {
        return "type";
    }
    
    public boolean isReferenceType() {
        return false;
    }
    
    public boolean isInterfaceType() {
        return false;
    }
    
    public boolean isDataType() {
        return false;
    }
    
    public boolean isNullType() {
        return false;
    }
    
    public boolean isTypeParameter() {
        return false;
    }
    
    public boolean isUnknownType() {
        return false;
    }
    
    public boolean isFutureType() {
        return false;
    }
    
    public boolean isBoolType() {
        return false;
    }
    
    public boolean isStringType() {
        return false;
    }
    
    public boolean isIntType() {
        return false;
    }
    
 	public boolean isAnyType() {
		return false;
	}
    
 	public MethodSig lookupMethod(String name) {
 	    return null;
 	}
 	
    public boolean equals(Object o) {
        if (o == null) return false;
        if (o.getClass() != this.getClass())
            return false;
        return true;
    }
    
    public boolean isSubtypeOf(Type t) {
   	 // default implementation 
   	 return equals(t);
    }
}
