package abs.frontend.typechecker.locationtypes;

import java.util.ArrayList;
import java.util.List;

public abstract class LocationType {
    public static final LocationType FAR = new Far();
    public static final LocationType NEAR = new Near();
    public static final LocationType SOMEWHERE = new Somewhere();
    public static final LocationType NOTYPE = new NoType();
    public static final LocationType BOTTOM = new Bottom();
   
    private final static class Far extends LocationType { }
    private final static class Near extends LocationType { }
    private final static class Somewhere extends LocationType { }
    private final static class NoType extends LocationType { }
    private final static class Bottom extends LocationType { }
    
    @Override
    public String toString() {
        if (isFar()) return "Far";
        if (isNear()) return "Near";
        if (isSomewhere()) return "Somewhere";
        return "NoType";
    }
    
    public final static class Parametric extends LocationType {
        List<LocationType> typeParams;
        public Parametric(List<LocationType> t) {
            this.typeParams = t;
        }
        
        @Override
        public LocationType adaptTo(LocationType to) {
            List<LocationType> adapted = new ArrayList<LocationType>();
            for (LocationType t : typeParams) {
                adapted.add(t.adaptTo(to));
            }
            return new Parametric(adapted);
        }
        
        @Override
        public boolean isSubtypeOf(LocationType t) {
            if (super.isSubtypeOf(t))
                return true;
            
            if (! (t instanceof Parametric))
                return false;
            
            Parametric p = (Parametric) t;
            if (p.typeParams.size() != this.typeParams.size())
                return false;
            
            for (int i = 0; i < typeParams.size(); i++) {
                if (!this.typeParams.get(i).isSubtypeOf(p.typeParams.get(i)))
                    return false;
            }
            return true;
        }
        
        @Override
        public String toString() {
            StringBuilder b = new StringBuilder();
            b.append("[");
            boolean first = true;
            for (LocationType t : typeParams) {
                if (first) first = false;
                else b.append(",");
                b.append(t.toString());
            }
            b.append("]");
            return b.toString();
        }
    }
    
    public static LocationType createFromName(String name) { 
        if (name.equals("Far"))
            return FAR;
        if (name.equals("Near"))
            return NEAR;
        if (name.equals("Somewhere"))
            return SOMEWHERE;
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
    
    public boolean isSubtypeOf(LocationType t) {
        return this == t || t == SOMEWHERE || this == BOTTOM;
    }
    
    public LocationType adaptTo(LocationType to) {
        if (isNoType() || isBottom())
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

        throw new IllegalArgumentException("Cannot use location type "+to+" to adapt to");
    }
    
}
