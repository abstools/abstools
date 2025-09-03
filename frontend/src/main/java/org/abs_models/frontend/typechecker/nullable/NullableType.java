package org.abs_models.frontend.typechecker.nullable;

import com.google.common.collect.ImmutableMap;
import org.abs_models.frontend.ast.VarOrFieldDecl;

public interface NullableType {
    /**
     * @param lhs - The left hand type
     * @param rhs - The right hand type
     * @return - Whether the assignment lhs = rhs would be correct
     */
    static boolean assignable(NullableType lhs, NullableType rhs) {
        if (lhs == PrimitiveNullableType.Nonnull) {
            return rhs == PrimitiveNullableType.Nonnull;
        }
        return true;
    }

    boolean assignableTo(NullableType n);

    NullableType getMostCommon(NullableType other);

    boolean isNonnull();

    boolean isNull();

    boolean isNullable();

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
