/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.typechecker.ext.AdaptDirection;

public class LocationType {
    public static final String LOCATION_KEY = "LOCATION_KEY";

    public static final LocationType FAR = new LocationType("Far");
    public static final LocationType NEAR = new LocationType("Near");
    public static final LocationType SOMEWHERE = new LocationType("Somewhere");
    public static final LocationType BOTTOM = new LocationType("Bottom");
    public static final LocationType INFER = new LocationType("Infer");

    public static final LocationType[] ALL_BASIC_TYPES = {FAR, NEAR, SOMEWHERE, BOTTOM, INFER};
    public static final LocationType[] ALL_CONCRETE_BASIC_TYPES = {FAR, NEAR, SOMEWHERE, BOTTOM};
    public static final LocationType[] ALL_CONCRETE_USER_TYPES = {FAR, NEAR, SOMEWHERE};
    public static final LocationType[] ALL_USER_TYPES = {FAR, NEAR, SOMEWHERE, INFER};

    private final String name;

    private LocationType(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }

    public static LocationType createFromName(String name) {
        for (LocationType t : ALL_BASIC_TYPES) {
            if (t.name.equals(name))
                return t;
        }
        throw new IllegalArgumentException(name + " is not a location type");
    }

    public boolean isSubtypeOfFarAdapted(LocationType t) {
        if (this.isBottom()) {
            return true;
        }
        if (this.isNear() || this.isFar() || this.isSomewhere()) {
            return t.isFar() || t.isSomewhere();
        }
        throw new IllegalArgumentException("Cannot use location type " + this + " to check subtypeOfFar");
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
            || t.isSomewhere();
    }

    public LocationType adaptTo(LocationType to, AdaptDirection dir) {
        // Bottom stays the same
        if (isBottom()) return this;

        // If we adapt to FAR and we are NEAR, we are on a different COG than the caller (=> FAR)
        // else we are somewhere
        if (to.isFar()) return isNear() ? FAR : SOMEWHERE;

        if (to.isNear()) return this;

        if (to.isSomewhere()) return SOMEWHERE;

        if (to.isBottom()) return SOMEWHERE;

        throw new IllegalArgumentException("Cannot use location type " + to + " to adapt to");
    }

    public String toAnnotationString() {
        return "[" + toString() + "] ";
    }
}
