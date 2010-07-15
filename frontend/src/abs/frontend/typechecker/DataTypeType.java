package abs.frontend.typechecker;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import abs.frontend.ast.DataTypeDecl;

public class DataTypeType extends Type {
    private final DataTypeDecl decl;
    private final List<Type> typeArgs = new ArrayList<Type>();
    
    public DataTypeType(DataTypeDecl decl) {
        this(decl, new Type[0]);
    }
    
    public DataTypeType(DataTypeDecl decl, Type... typeArgs) {
        this.decl = decl;
        for (Type t : typeArgs) {
            this.typeArgs.add(t);
        }
    }
    
    public List<Type> getTypeArgs() {
        return Collections.unmodifiableList(typeArgs);
    }
    
    public Type getTypeArg(int i) {
        return typeArgs.get(i);
    }
    
    public boolean hasTypeArgs() {
        return !typeArgs.isEmpty();
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
    
    public boolean isFutureType() {
        return decl.getName().equals("Fut");
    }

    public boolean isBoolType() {
        return decl.getName().equals("Bool");
    }

    public boolean isUnitType() {
        return decl.getName().equals("Unit");
    }

    public boolean isIntType() {
        return decl.getName().equals("Int");
    }

    public boolean isStringType() {
        return decl.getName().equals("String");
    }
    
    public String toString() {
        return decl.getName();
    }
    
    
    

}
