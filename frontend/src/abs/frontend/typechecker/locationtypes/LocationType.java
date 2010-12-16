package abs.frontend.typechecker.locationtypes;

import java.util.ArrayList;
import java.util.List;

public class LocationType {
    public static final String LOCATION_KEY = "LOCATION_KEY";
    
    public static final LocationType FAR = new LocationType("Far");
    public static final LocationType NEAR = new LocationType("Near");
    public static final LocationType SOMEWHERE = new LocationType("Somewhere");
    public static final LocationType BOTTOM = new LocationType("Bottom");
    public static final LocationType INFER = new LocationType("Infer");
    public static final LocationType NOTYPE = new LocationType("NoType");
    public static final LocationType UNBOUND = new LocationType("Unbound");
    
    public static final LocationType[] ALLTYPES = {FAR, NEAR, SOMEWHERE, BOTTOM, INFER, NOTYPE, UNBOUND};
    public static final LocationType[] ALLVISTYPES = {FAR, NEAR, SOMEWHERE, BOTTOM};
    public static final LocationType[] ALLCONCRETEUSERTYPES = {FAR, NEAR, SOMEWHERE};
    public static final LocationType[] ALLUSERTYPES = {FAR, NEAR, SOMEWHERE, INFER};
   
    private String name;
    
    private LocationType(String name) {
        this.name = name;
    }
    
    @Override
    public String toString() {
        return name;
    }
    
    public static LocationType createFromName(String name) {
        for (LocationType t : ALLTYPES) {
            if (t.name.equals(name))
                return t;
        }
        throw new IllegalArgumentException(name+" is not a location type");
    }
    
    public boolean isFar() { 
        return this == FAR;
    }
    
    public boolean isInfer() { 
        return this == INFER;
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
        return this.isUnbound() 
            || this == t 
            || this.isBottom() 
            || t.isUnbound() 
            || t.isSomewhere();
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
