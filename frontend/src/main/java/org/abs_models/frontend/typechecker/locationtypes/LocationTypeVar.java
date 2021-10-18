package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.typechecker.Type;

import java.util.HashMap;
import java.util.Map;

/**
 * Used to abstract location types for inference
 */
public class LocationTypeVar {
    private ASTNode<?> node;

    private static int counter = 0;

    private int id = counter++;

    public static final String LOCATION_VAR_KEY = "LOCATION_VAR_KEY";

    public static final LocationTypeVar NEAR = new LocationTypeVar(null);
    public static final LocationTypeVar FAR = new LocationTypeVar(null);
    public static final LocationTypeVar SOMEWHERE = new LocationTypeVar(null);
    public static final LocationTypeVar BOTTOM = new LocationTypeVar(null);

    public LocationTypeVar(ASTNode<?> node) {
        this.node = node;
    }

    public boolean isPrimitive() {
        return this == NEAR || this == FAR || this == SOMEWHERE || this == BOTTOM;
    }

    public boolean isLocationType() {
        return isPrimitive();
    }

    public LocationType asLocationType() {
        if (this == BOTTOM) return LocationType.BOTTOM;
        if (this == NEAR) return LocationType.NEAR;
        if (this == FAR) return LocationType.FAR;
        if (this == SOMEWHERE) return LocationType.SOMEWHERE;

        throw new IllegalArgumentException("Cannot convert " + this + " to location type");
    }

    public static LocationTypeVar getVar(Type t) {
        return (LocationTypeVar) t.getMetaData(LOCATION_VAR_KEY);
    }

    public static void setVar(Type t, LocationTypeVar v) {
        t.addMetaData(LOCATION_VAR_KEY, v);
    }

    public static LocationTypeVar getFromLocationType(LocationType lt) {
        if (lt.isBottom()) return BOTTOM;
        if (lt.isNear()) return NEAR;
        if (lt.isFar()) return FAR;
        if (lt.isSomewhere()) return SOMEWHERE;
        throw new IllegalArgumentException();
    }

    public int getId() {
        return id;
    }

    @Override
    public String toString() {
        if (isPrimitive()) {
            if (this == NEAR) return "NEAR";
            if (this == FAR) return "FAR";
            if (this == SOMEWHERE) return "SOMEWHERE";
            if (this == BOTTOM) return "BOTTOM";
        }
        String pos = node == null ? "" : "@" + node.getPositionString();
        return "v" + id + pos;
    }

}
