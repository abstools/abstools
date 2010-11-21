package abs.frontend.typechecker;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.List;
import abs.frontend.ast.MethodSig;

public class UnionType extends ReferenceType {
    private final java.util.List<InterfaceType> types;
    private final ClassDecl originatingClass;

    public UnionType(ClassDecl classDecl, List<InterfaceTypeUse> types) {
        this.types = new ArrayList<InterfaceType>();
        for (InterfaceTypeUse t : types) {
            if (t.getType() instanceof InterfaceType) // maybe UnkownType
                this.types.add((InterfaceType) t.getType());
        }
        originatingClass = classDecl;
    }

    public UnionType(ClassDecl classDecl, InterfaceType... types) {
        this.types = new ArrayList<InterfaceType>();
        this.types.addAll(Arrays.asList(types));
        originatingClass = classDecl;
    }

    public java.util.List<InterfaceType> getTypes() {
        return Collections.unmodifiableList(types);
    }

    public InterfaceType getType(int i) {
        return types.get(i);
    }

    @Override
    public boolean equals(Object o) {
        if (!super.equals(o))
            return false;
        if (!(o instanceof UnionType))
            return false;
        UnionType t = (UnionType) o;
        return t.types.equals(this.types);
    }

    @Override
    public int hashCode() {
        return types.hashCode();
    }

    @Override
    public boolean isAssignable(Type t) {
        if (super.isAssignable(t))
            return true;

        for (InterfaceType it : types) {
            if (it.isAssignable(t))
                return true;
        }
        return false;
    }

    @Override
    public MethodSig lookupMethod(String name) {
        for (InterfaceType t : types) {
            MethodSig s = t.lookupMethod(name);
            if (s != null)
                return s;
        }
        return null;
    }

    @Override
    public String toString() {
        StringBuilder buf = new StringBuilder();
        buf.append("UnionType{");
        boolean first = true;
        for (InterfaceType t : types) {
            if (!first) {
                buf.append(", ");
                first = false;
            }
            buf.append(t.toString());
        }
        buf.append(" }");
        return buf.toString();
    }

    @Override
    public boolean isUnionType() {
        return true;
    }

    public ClassDecl getOriginatingClass() {
        return originatingClass;
    }
}
