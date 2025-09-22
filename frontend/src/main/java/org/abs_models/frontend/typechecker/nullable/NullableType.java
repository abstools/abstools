package org.abs_models.frontend.typechecker.nullable;

import com.google.common.collect.ImmutableMap;
import org.abs_models.frontend.ast.VarOrFieldDecl;

/// A behavioral type denoting the nullability of types.
/// It is either primitive ([PrimitiveNullableType]) or a data type ([DataTypeNullableType]).
public interface NullableType {
    /// @param lhs - The left hand type
    /// @param rhs - The right hand type
    /// @return - Whether the assignment lhs = rhs would be correct
    static boolean assignable(NullableType lhs, NullableType rhs) {
        if (lhs == PrimitiveNullableType.Nonnull) {
            return rhs == PrimitiveNullableType.Nonnull;
        }
        return true;
    }

    /// Whether this nullable type is assignable to `n`.
    boolean assignableTo(NullableType n);

    /// The join operator of the type lattice. The rules are:
    /// - UNKNOWN x NT = NT
    /// - NULL x NULLABLE = NULLABLE
    /// - NULL x NONNULL = NULLABLE
    /// - NULLABLE x NONNULL = NULLABLE
    /// - (NT_1, ..., NT_n) x (NT'_1, ..., NT'_n) = (NT_1 x NT'_1, ..., NT_n x NT'_n).
    NullableType getMostCommon(NullableType other);

    /// True iff we are [PrimitiveNullableType#Nonnull].
    boolean isNonnull();

    /// True iff we are [PrimitiveNullableType#Null].
    boolean isNull();

    /// True iff we are [PrimitiveNullableType#Nullable].
    boolean isNullable();

    /// Returns a builder that is the "intersection" of the two immutable maps.
    ///
    /// If both maps contain a nullable type for a key, the join of both entries is used.
    static ImmutableMap.Builder<VarOrFieldDecl, NullableType> intersect(ImmutableMap<VarOrFieldDecl, NullableType> map1, ImmutableMap<VarOrFieldDecl, NullableType> map2) {
        var builder = new ImmutableMap.Builder<VarOrFieldDecl, NullableType>();
        for (var e1 : map1.entrySet()) {
            var decl = e1.getKey();
            var nt1 = e1.getValue();
            var nt2 = map2.get(decl);
            if (nt1 == null) {
                builder = builder.put(decl, nt2);
            } else if (nt2 == null) {
                builder = builder.put(decl, nt1);
            } else {
                builder = builder.put(decl, nt1.getMostCommon(nt2));
            }
        }
        return builder;
    }
}
