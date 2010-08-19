package abs.frontend.typechecker;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import abs.frontend.ast.DataTypeDecl;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.TypeParameterDecl;

public class DataTypeType extends Type {
    private final DataTypeDecl decl;
    private final List<Type> typeArgs = new ArrayList<Type>();
    
    public DataTypeType(DataTypeDecl decl) {
        this(decl, new Type[0]);
    }

    public DataTypeType(DataTypeDecl decl, abs.frontend.ast.List<DataTypeUse> typeUses) {
        this(decl, TypeCheckerHelper.getTypesFromDataTypeUse(typeUses).toArray(new Type[0]));
    }
    
    public DataTypeType(DataTypeDecl decl, Type... typeArgs) {
        this.decl = decl;
        for (Type t : typeArgs) {
            this.typeArgs.add(t);
        }
    }

    public DataTypeType(DataTypeDecl decl, List<Type> typeArgs) {
       this.decl = decl;
       this.typeArgs.addAll(typeArgs);
   }
    
    public List<Type> getTypeArgs() {
        return Collections.unmodifiableList(typeArgs);
    }
    
    public int numTypeArgs() {
        return typeArgs.size();
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
   	 StringBuffer buf = new StringBuffer(decl.getName());
   	 if (hasTypeArgs()) {
   		 buf.append('<');
   		 boolean first = true;
   		 for (Type t : typeArgs) {
   			 if (!first)
   				 buf.append(',');
   			 if (t != null)
   			     buf.append(t.toString());
   			 else
   			     buf.append("<unknown>");
   			 first = false;
   		 }
   		 buf.append('>');
   	 }
   	 
   	 return buf.toString();
    }
    
    public Type substituteTypeParams(Type t) {
        
        if (!hasTypeArgs())
            return t;

        if (!(t.isDataType() || t.isTypeParameter()))
            return t;
        
        Map<TypeParameterDecl,Type> substitution = getSubstitutionMap();

        if (t.isDataType()) {
            DataTypeType dt = (DataTypeType) t;
            List<Type> substitutedArgs = new ArrayList<Type>();
            for (Type arg : dt.getTypeArgs()) {
                if (!arg.isTypeParameter()) 
                    substitutedArgs.add(arg);
            
                TypeParameter tp = (TypeParameter) arg;
                substitutedArgs.add(substitution.get(tp.getDecl()));
            }
            return new DataTypeType(dt.getDecl(),substitutedArgs.toArray(new Type[0]));
        } else {
            TypeParameter tp = (TypeParameter) t;
            return substitution.get(tp.getDecl());
        }
    }

    private Map<TypeParameterDecl, Type> getSubstitutionMap() {
        Map<TypeParameterDecl, Type> substitution = new HashMap<TypeParameterDecl, Type>();
        ParametricDataTypeDecl pd = (ParametricDataTypeDecl) decl;
        for (int i = 0; i < numTypeArgs(); i++) {
            substitution.put(pd.getTypeParameter(i), getTypeArg(i));
        }
        
        return substitution;
    }
    

}
