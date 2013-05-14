/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import abs.frontend.ast.*;

public class DataTypeType extends Type  {
    private final DataTypeDecl decl;
    private final List<Type> typeArgs = new ArrayList<Type>();

    @Override
    public Type copy() {
        return new DataTypeType(decl,typeArgs);
    }
    
    @Override
    public Type fullCopy() {
        List<Type> typeArgCopy = new ArrayList<Type>();
        for (Type t : typeArgs) {
            typeArgCopy.add(t.fullCopy());
        }
        Type copy = new DataTypeType(decl,typeArgCopy);
        copy.metaData.putAll(metaData);
        return copy;
    }
    
    public DataTypeType(DataTypeDecl decl) {
        this(decl, new Type[0]);
    }

    public DataTypeType(DataTypeDecl decl, ParametricDataTypeUse typeUses) {
        this(decl, typeUses.getTypes().toArray(new Type[0]));
    }

    public DataTypeType(DataTypeDecl decl, Type... typeArgs) {
        this.decl = decl;
        for (Type t : typeArgs) {
            if (t == null)
                throw new IllegalArgumentException("Type argument was null");
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
    
    public DataTypeType withTypeArgs(Type... typeArgs) {
        DataTypeType copy = (DataTypeType) copy();
        copy.typeArgs.addAll(Arrays.asList(typeArgs));
        if (copy.typeArgs.contains(null))
            throw new IllegalArgumentException("One type argument was null");
        return copy;
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
        if (!(o instanceof DataTypeType))
            return false;

        DataTypeType t = (DataTypeType) o;
        if (!t.decl.equals(this.decl))
            return false;
        for (int i = 0; i < numTypeArgs(); i++) {
            if (!getTypeArg(i).equals(t.getTypeArg(i)))
                return false;
        }
        return true;
    }

    @Override
    public boolean isAssignable(Type t) {
        return this.isAssignable(t, true);
    }

    @Override
    public boolean isAssignable(Type t, boolean considerSubtyping) {
        if (super.isAssignable(t))
            return true;

        if (!(t instanceof DataTypeType))
            return false;

        DataTypeType dt = (DataTypeType) t;
        if (!dt.decl.equals(this.decl)) {
            // Int and Rat are cross-assignable, with implicit truncation
            if (this.isNumericType() && dt.isNumericType()) return true;
            return false;
        }
        for (int i = 0; i < numTypeArgs(); i++) {
            if (!getTypeArg(i).isAssignable(dt.getTypeArg(i), false))
                return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return decl.hashCode();
    }

    public DataTypeDecl getDecl() {
        return decl;
    }

    @Override
    public boolean isAnnotationType() {
        for (Annotation a :decl.getAnnotations()) {
            try {
                Type t = a.getType();
                if (t.isDataType()) {
                    DataTypeType dt = (DataTypeType) t;
                    if (dt.getDecl().getName().equals("Annotation")) {
                        if (a.getValue() instanceof DataConstructorExp) {
                            DataConstructorExp dexp = (DataConstructorExp) a.getValue();
                            if (dexp.getDecl().getName().equals("TypeAnnotation"))
                                return true;
                        }
                    }
                }
            } catch (TypeCheckerException e) {
                // ignore illegally typed annotations (for now at least)
                continue;
            }
        }
        return false;
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

    public boolean isRatType() {
        return decl.getName().equals("Rat");
    }

    public boolean isStringType() {
        return decl.getName().equals("String");
    }

    public String toString() {
        StringBuffer buf = new StringBuffer(super.toString());
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
    
    @Override
    public String getModuleName() {
        return decl.getModuleDecl().getName();
    }

    @Override
    public String getSimpleName() {
        return decl.getName();
    }

    

    public Type substituteTypeParams(Type t) {
        if (!hasTypeArgs())
            return t;

        if (!(t.isDataType() || t.isTypeParameter()))
            return t;

        Map<String, Type> substitution = getSubstitutionMap();

        if (t.isDataType()) {
            DataTypeType dt = (DataTypeType) t;
            List<Type> substitutedArgs = new ArrayList<Type>();
            for (Type arg : dt.getTypeArgs()) {
                if (!arg.isTypeParameter()) {
                    substitutedArgs.add(arg);
                } else {
                    TypeParameter tp = (TypeParameter) arg;
                    Type st = substitution.get(tp.getDecl().getName());
                    if (st == null)
                        throw new NullPointerException();
                    substitutedArgs.add(st);
                }
            }
            return new DataTypeType(dt.getDecl(), substitutedArgs.toArray(new Type[0]));
        } else {
            TypeParameter tp = (TypeParameter) t;
            return substitution.get(tp.getDecl().getName());
        }
    }

    private Map<String, Type> getSubstitutionMap() {
        Map<String, Type> substitution = new HashMap<String, Type>();
        ParametricDataTypeDecl pd = (ParametricDataTypeDecl) decl;
        for (int i = 0; i < numTypeArgs(); i++) {
            substitution.put(pd.getTypeParameter(i).getName(), getTypeArg(i));
        }

        return substitution;
    }

    @Override
    public Type applyBinding(Map<TypeParameter, Type> binding) {
        if (hasTypeArgs()) {
            List<Type> argTypes = TypeCheckerHelper.applyBindings(binding,getTypeArgs());
            return new DataTypeType(getDecl(), argTypes);
        } else
            return super.applyBinding(binding);
    }
    
    @Override
    public DataTypeUse toUse() {
        if (hasTypeArgs()) {
            abs.frontend.ast.List<DataTypeUse> ls = new abs.frontend.ast.List();
            for (Type arg : getTypeArgs()) {
                ls.add(arg.toUse());
            }
            return new ParametricDataTypeUse(getSimpleName(), new abs.frontend.ast.List(), ls);
        } else {
            return new DataTypeUse(getSimpleName(), new abs.frontend.ast.List()); 
        }
    }
}
