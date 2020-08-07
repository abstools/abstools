package org.abs_models.frontend.typechecker.locationtypes;

public enum Scope {
    METHOD_LOCAL_FAR,
    CLASS_LOCAL_FAR,
    COMPILATION_UNIT_LOCAL_FAR,
    MODULE_LOCAL_FAR,
    GLOBAL_FAR;

    public String prefix() {
        switch (this) {
            case METHOD_LOCAL_FAR: return "M";
            case CLASS_LOCAL_FAR: return "C";
            case COMPILATION_UNIT_LOCAL_FAR: return "CU";
            case MODULE_LOCAL_FAR: return "MOD";
            case GLOBAL_FAR: return "G";
        }
        throw new IllegalArgumentException("Invalid location type: " + this);
    }
}
