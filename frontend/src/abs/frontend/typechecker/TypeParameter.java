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
        if (!super.equals(o))
            return false;
        TypeParameter t = (TypeParameter) o;
        return t.decl.equals(this.decl);
    }
    
    @Override
    public int hashCode() {
        return decl.hashCode();
    }
}
