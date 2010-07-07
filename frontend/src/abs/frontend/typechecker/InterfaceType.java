package abs.frontend.typechecker;

import abs.frontend.ast.InterfaceDecl;

public class InterfaceType extends ReferenceType {
    private final InterfaceDecl decl;
    
    public InterfaceType(InterfaceDecl decl) {
        this.decl = decl;
    }
    
    public InterfaceDecl getDecl() {
        return decl;
    }
    
    @Override
    public boolean isInterfaceType() {
        return true;
    }
    
    @Override
    public boolean equals(Object o) {
        if (!super.equals(o))
            return false;
        InterfaceType t = (InterfaceType) o;
        return t.decl.equals(this.decl);
    }
    
    @Override
    public int hashCode() {
        return decl.hashCode();
    }
    
}
