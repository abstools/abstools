package org.abs_models.frontend.typechecker.nullable;

import org.abs_models.frontend.ast.Annotation;
import org.abs_models.frontend.ast.DataConstructorExp;

/**
 * The nullable types of expressions which may actually be null.
 */
public enum PrimitiveNullableType implements NullableType {
    /**
     * The expression is always null
     */
    Null,
    /**
     * The expression or declaration can never be null
     */
    Nonnull,
    /**
     * The expression or declaration may be null
     */
    Nullable,
    /**
     * Has no null type because it is not a reference or data type type.
     */
    NonApplicable,
    Unknown;

    public static final PrimitiveNullableType[] USER_TYPES = {Nonnull, Nullable};

    /**
     * Whether this is assignable to `n`
     * @param n - The type to check against
     * @return - True iff this is assignable to `n`
     */
    @Override
    public boolean assignableTo(NullableType n) {
        return NullableType.assignable(n, this);
    }

    /**
     * Gets the most common nullable tyoe between this and `other`
     * @param other - The other type
     * @return - The most common type
     */
    @Override
    public NullableType getMostCommon(NullableType other) {
        if (this == NonApplicable || other == NonApplicable)
            return NonApplicable;
        if (this == Nullable || other == Nullable) {
            return Nullable;
        }

        if (this == Unknown)
            return other;
        if (other == Unknown)
            return this;

        if (this == Nonnull) {
            if (other == Nonnull) {
                return Nonnull;
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

    /**
     * Tries to compute the nullable type from a string
     * @param name - The name of the type
     * @return - The converted type
     */
    public static PrimitiveNullableType fromName(String name) {
        if (name.equals("Nonnull")) {
            return PrimitiveNullableType.Nonnull;
        }
        return PrimitiveNullableType.Nullable;
    }

    /**
     * @return - Whether `this` is NonNull
     */
    @Override
    public boolean isNonnull() {
        return this == Nonnull;
    }

    /**
     * @return - Whether `this` is Null
     */
    @Override
    public boolean isNull() {
        return this == Null;
    }

    /**
     * @return - Whether `this` is Nullable
     */
    @Override
    public boolean isNullable() {
        return this == Nullable;
    }

    /**
     *
     * @return - The annotation for this type
     */
    public Annotation toAnnotation() {
        if (isNull()) {
            throw new IllegalArgumentException("Cannot turn Null into annotation.");
        }
        return new Annotation(new DataConstructorExp(toString(), new org.abs_models.frontend.ast.List<>()));
    }
}
