package abs.frontend.typechecker;

public abstract class Type {
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
    
    public boolean equals(Object o) {
        if (o == null) return false;
        if (o.getClass() != this.getClass())
            return false;
        return true;
    }
}
