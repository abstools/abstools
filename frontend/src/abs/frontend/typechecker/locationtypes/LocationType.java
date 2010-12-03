package abs.frontend.typechecker.locationtypes;

public enum LocationType {
    FAR, NEAR, SOMEWHERE;
   
    public boolean isSubtypeOf(LocationType t) {
        return this == t || t == SOMEWHERE;
    }
    
    public LocationType adaptTo(LocationType to) {
        switch (to) {
        case FAR: return this == NEAR ? FAR : SOMEWHERE;
        case NEAR: return this;
        case SOMEWHERE: return SOMEWHERE;
        }
        throw new IllegalArgumentException("Convince the Java type checker");
    }
    
    @Override
    public String toString() {
        return this.name().substring(0,1)+this.name().substring(1).toLowerCase();
    }
}
