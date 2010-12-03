package abs.frontend.typechecker;

import abs.frontend.ast.TypeParameterDecl;

public class TypeParameter extends Type {
    private final TypeParameterDecl decl;

    public TypeParameter(TypeParameterDecl decl) {
        this.decl = decl;
    }

    public TypeParameterDecl getDecl() {
        return decl;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof Type))
            return false;
        Type t = (Type) o;
        if (t.canBeBoundTo(this))
            return true;

        if (!(t instanceof TypeParameter))
            return false;

        TypeParameter tp = (TypeParameter) t;
        return tp.decl.equals(this.decl);
    }

    @Override
    public int hashCode() {
        return decl.hashCode();
    }

    @Override
    public String toString() {
        return decl.getName();
    }

    @Override
    public boolean isTypeParameter() {
        return true;
    }

    @Override
    public String getSimpleName() {
        return getDecl().getName();
    }

    @Override
    protected Type copy() {
        return new TypeParameter(decl);
    }
}
