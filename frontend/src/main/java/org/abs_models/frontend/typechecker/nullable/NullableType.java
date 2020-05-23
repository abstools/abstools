package org.abs_models.frontend.typechecker.nullable;

public enum NullableType {
    Null, NonNull, Nullable, None;

    public boolean assignableTo(NullableType n) {
        return NullableType.assignable(n, this);
    }

    public static boolean assignable(NullableType lhs, NullableType rhs) {
        if (lhs == NullableType.NonNull) {
            return rhs == NullableType.NonNull;
        }
        if (lhs == NullableType.Nullable) {
            return rhs != NullableType.None;
        }
        if (lhs == NullableType.Null) {
            return rhs == NullableType.Null;
        }
        return true;
    }
}
