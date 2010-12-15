package abs.frontend.typechecker.locationtypes;

import java.util.ArrayList;
import java.util.List;

public abstract class LocationType {
    public static final String LOCATION_KEY = "LOCATION_KEY";
    
    public static final LocationType FAR = new Far();
    public static final LocationType NEAR = new Near();
    public static final LocationType SOMEWHERE = new Somewhere();
    public static final LocationType NOTYPE = new NoType();
    public static final LocationType UNBOUND = new Unbound();
    public static final LocationType BOTTOM = new Bottom();
    
    public static final LocationType[] ALLTYPES = {FAR, NEAR, SOMEWHERE, BOTTOM};
    
    public static final LocationType[] ALLVISTYPES = {FAR, NEAR, SOMEWHERE};
   
    private final static class Far extends LocationType { }
    private final static class Near extends LocationType { }
    private final static class Somewhere extends LocationType { }

    // to give unbound type variables a location type
    private final static class Unbound extends LocationType { }
    
    // used to give data types a location type
    private final static class NoType extends LocationType { }
    private final static class Bottom extends LocationType { }
    
    @Override
    public String toString() {
        if (isFar()) return "Far";
        if (isNear()) return "Near";
        if (isSomewhere()) return "Somewhere";
        if (isBottom()) return "Bottom";
        if (isUnbound()) return "Unbound";
        return "NoType";
    }
    
    public static LocationType createFromName(String name) { 
        if (name.equals("Far"))
            return FAR;
        if (name.equals("Near"))
            return NEAR;
        if (name.equals("Somewhere"))
            return SOMEWHERE;
        if (name.equals("Bottom"))
            return BOTTOM;
        throw new IllegalArgumentException(name+" is not a location type");
    }
    
    public boolean isFar() { 
        return this == FAR;
    }
    
    public boolean isNear() {
        return this == NEAR;
    }
    
    public boolean isSomewhere() {
        return this == SOMEWHERE;
    }
    
    public boolean isNoType() {
        return this == NOTYPE;
    }

    public boolean isBottom() {
        return this == BOTTOM;
    }
    
    public boolean isUnbound() {
        return this == UNBOUND;
    }
    
    public boolean isSubtypeOf(LocationType t) {
        return this == UNBOUND || t == UNBOUND || this == t || t == SOMEWHERE || this == BOTTOM;
    }
    
    public boolean isSubtypeOfFarAdapted(LocationType t) {
        if (this.isBottom()) {
            return true;
        }
        if (this.isNear() || this.isFar() || this.isSomewhere()) {
            return t.isFar() || t.isSomewhere();
        }
        throw new IllegalArgumentException("Cannot use location type "+this+" to check subtypeOfFar");
    }
    
    public LocationType adaptTo(LocationType to) {
        if (isNoType() || isBottom() || isUnbound())
            return this;
        
        if (to.isFar()) {
            return this.isNear() ? FAR : SOMEWHERE; 
        }
        
        if (to.isNear()) {
            return this;
        }
        
        if (to.isSomewhere()) {
            return SOMEWHERE;
        }
        
        if (to.isBottom()) {
            return SOMEWHERE;
        }
        
        throw new IllegalArgumentException("Cannot use location type "+to+" to adapt to");
    }
}
