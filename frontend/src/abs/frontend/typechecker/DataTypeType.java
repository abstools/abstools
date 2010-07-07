package abs.frontend.typechecker;

import abs.frontend.ast.DataTypeDecl;

public class DataTypeType extends Type {
    private final DataTypeDecl decl;
    
    public DataTypeType(DataTypeDecl decl) {
        this.decl = decl;
    }
    
    @Override
    public boolean isDataType() {
        return true;
    }
    
    @Override
    public boolean equals(Object o) {
        if (!super.equals(o))
            return false;
        DataTypeType t = (DataTypeType) o;
        return t.decl.equals(this.decl);
    }
    
    @Override
    public int hashCode() {
        return decl.hashCode();
    }

    public DataTypeDecl getDecl() {
        return decl;
    }
    
}
