package org.abs_models.frontend.typechecker.nullable;

public enum NullableType {
    Null, NonNull, Nullable;

    public boolean assignableTo(NullableType n) {
        return NullableType.assignable(n, this);
    }

    public NullableType getMostCommon(NullableType other) {
        if (this == Nullable || other == Nullable) {
            return Nullable;
        }

        if (this == NonNull) {
            if (other == NonNull) {
                return NonNull;
            }
            return Nullable;
        }

        if (this == Null) {
            if (other == Null) {
                return Null;
            }
        }
        return Nullable;
    }

    public static boolean assignable(NullableType lhs, NullableType rhs) {
        if (lhs == NullableType.NonNull) {
            return rhs == NullableType.NonNull;
        }
        return true;
    }

    public static NullableType fromName(String name) {
        if (name.equals("NonNull")) {
            return NullableType.NonNull;
        }
        return NullableType.Nullable;
    }
}
