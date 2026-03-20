package org.abs_models.frontend.typechecker.nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * The nullable types of expressions which cannot be null, but their type parameters may be.
 * <p></p>
 * E.g., an expression of type {@code Maybe<Object>} can never be null, but its value can be.
 */
public class DataTypeNullableType implements NullableType {
    ///  For data types without parameters.
    public static DataTypeNullableType EMPTY = new DataTypeNullableType(new ArrayList<>());
    ///  Useful for e.g. Destiny
    public static DataTypeNullableType DT_OF_EMPTY = new DataTypeNullableType(List.of(EMPTY));

    private final List<NullableType> params;

    public DataTypeNullableType(List<NullableType> params) {
        this.params = params;
    }

    public DataTypeNullableType(NullableType nt) {
        this(List.of(nt));
    }

    public List<NullableType> getParams() {
        return params;
    }

    public NullableType getParam(int i) {
        return params.get(i);
    }

    @Override
    public boolean assignableTo(NullableType n) {
        if (!(n instanceof DataTypeNullableType dnt))
            return false;
        if (getParams().size() != dnt.getParams().size()) return false;
        for (int i = 0; i < getParams().size(); ++i) {
            if (!getParam(i).assignableTo(dnt.getParam(i))) return false;
        }
        return true;
    }

    @Override
    public DataTypeNullableType getMostCommon(NullableType other) {
        if (other == PrimitiveNullableType.Unknown) return this;
        if (!(other instanceof DataTypeNullableType dnt))
            throw new IllegalArgumentException("Expected data nullable type, got: " + other);
        if (getParams().size() != dnt.getParams().size())
            throw new IllegalArgumentException("Expected same number of nullable type parameters: " + getParams().size() + " =/= " + dnt.getParams().size());
        var newParams = new ArrayList<NullableType>(getParams().size());
        for (int i = 0; i < getParams().size(); ++i) {
            newParams.add(getParam(i).getMostCommon(dnt.getParam(i)));
        }
        return new DataTypeNullableType(newParams);
    }

    @Override
    public boolean isNonnull() {
        return false;
    }

    @Override
    public boolean isNull() {
        return false;
    }

    @Override
    public boolean isNullable() {
        return false;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof DataTypeNullableType o)) return false;
        if (params.size() != o.params.size()) return false;
        for (int i = 0; i < params.size(); ++i) {
            if (params.get(i) == null) {
                if (o.params.get(i) != null) return false;
                continue;
            };
            if (!params.get(i).equals(o.params.get(i))) return false;
        }
        return true;
    }

    @Override
    public String toString() {
        var sb = new StringBuilder();
        sb.append('<');
        for (int i = 0; i < params.size(); ++i) {
            if (i > 0) sb.append(", ");
            sb.append(params.get(i).toString());
        }
        sb.append('>');
        return sb.toString();
    }
}
