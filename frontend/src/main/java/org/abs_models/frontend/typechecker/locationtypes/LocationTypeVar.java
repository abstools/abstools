package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.typechecker.Type;

public class LocationTypeVar {
    public static final String LOCATION_VAR_KEY = "LOCATION_VAR_KEY";

    public static final LocationTypeVar NEAR = new LocationTypeVar(null);
    public static final LocationTypeVar FAR = new LocationTypeVar(null);
    public static final LocationTypeVar SOMEWHERE = new LocationTypeVar(null);
    public static final LocationTypeVar BOTTOM = new LocationTypeVar(null);

    private ASTNode<?> node;
    private LocationType.ParameterizedFarType farType;

    private static int counter = 0;

    private int index = counter++;

    public LocationTypeVar(ASTNode<?> node) {
        this.node = node;
    }

    public LocationTypeVar(ASTNode<?> node, LocationType.ParameterizedFarType farType) {
        this.node = node;
        this.farType = farType;
    }

    public boolean isPrimitive() {
        return this == NEAR || this == FAR || this == SOMEWHERE || this == BOTTOM;
    }

    public boolean isParametricFar() {
        // TODO
        return false;
    }

    public boolean isLocationType() {
        return isPrimitive() || isParametricFar();
    }

    public LocationType asLocationType() {
        // TODO
        return null;
    }

    public static LocationTypeVar getVar(Type t) {
        return (LocationTypeVar) t.getMetaData(LOCATION_VAR_KEY);
    }

    public static void setVar(Type t, LocationTypeVar v) {
        t.addMetaData(LOCATION_VAR_KEY, v);
    }
}
