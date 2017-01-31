/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.locationtypes;

import abs.frontend.typechecker.ext.AdaptDirection;

public class LocationType {
    public static final String LOCATION_KEY = "LOCATION_KEY";
    
    public static final LocationType FAR = new LocationType("Far");
    public static final LocationType NEAR = new LocationType("Near");
    public static final LocationType SOMEWHERE = new LocationType("Somewhere");
    public static final LocationType BOTTOM = new LocationType("Bottom");
    public static final LocationType INFER = new LocationType("Infer");
    
    public static final LocationType[] ALLTYPES = {FAR, NEAR, SOMEWHERE, BOTTOM, INFER};
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
    
    public static LocationType createParametricFar(String s) {
        return new ParameterizedFarType(s);
    }
    
    private static class ParameterizedFarType extends LocationType {
        private ParameterizedFarType(String s) {
            super("Far(" + s + ")");
        }
        
        @Override
        public String toAnnoString() {
            return "[Far] ";
        }
        
        @Override
        public boolean equals(Object o) {
            if (o == null || !(o instanceof ParameterizedFarType)) {
                return false;
            } else {
                if (this == o) {
                    return true;
                } else {
                    ParameterizedFarType lt = (ParameterizedFarType) o;
                    return toString().equals(lt.toString());
                }
            }
        }
    }
    
    public boolean isParametricFar() {
        return this instanceof ParameterizedFarType;
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

    public boolean isBottom() {
        return this == BOTTOM;
    }
    
    public boolean isSubtypeOf(LocationType t) {
        return this == t
            || this.isBottom()
            || t.isSomewhere()
            || this.isParametricFar() && t.isFar();
    }
    
    public boolean isSubtypeOfFarAdapted(LocationType t) {
        if (this.isBottom()) {
            return true;
        }
        if (this.isNear() || this.isFar() || this.isParametricFar() || this.isSomewhere()) {
            return t.isFar() || t.isSomewhere() || (t.isParametricFar() && t == this);
        }
        throw new IllegalArgumentException("Cannot use location type "+this+" to check subtypeOfFar");
    }
    
    public LocationType adaptTo(LocationType to, AdaptDirection dir) {
        if (isBottom())
            return this;
        
        if (to.isFar()) {
            return this.isNear() ? FAR : SOMEWHERE; 
        }
        
        if (to.isParametricFar()) {
            if (this.isNear())
                return dir.isFrom() ? to : FAR;
            if (this.isParametricFar() && this != to) {
                return dir.isFrom() ? SOMEWHERE : this;
            }
            return SOMEWHERE; 
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

    public String toAnnoString() {
        return "["+toString()+"] ";
    }
}
