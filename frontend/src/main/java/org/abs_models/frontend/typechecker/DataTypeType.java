/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.nullable.NullCheckerExtension;
import org.abs_models.frontend.typechecker.nullable.NullableType;

public class DataTypeType extends Type {
    private final DataTypeDecl decl;
    private final List<Type> typeArgs = new ArrayList<>();

    @Override
    public Type copy() {
        return new DataTypeType(decl, typeArgs);
    }

    @Override
    public Type fullCopy() {
        List<Type> typeArgCopy = new ArrayList<>();
        for (Type t : typeArgs) {
            typeArgCopy.add(t.fullCopy());
        }
        Type copy = new DataTypeType(decl, typeArgCopy);
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
    public boolean hasReferences() {
        Set<DataTypeDecl> checkedDecls = new HashSet<>();
        return hasReferences(checkedDecls);
    }

    /**
     * Helper method to handle recursive data types.
     * This will also handle mutually recursive data types,
     * which would be difficult to handle in aspects (at least declarative ones).
     */
    private boolean hasReferences(Set<DataTypeDecl> checkedDecls) {
        if (
                isFutureType()
             || isAnyType() // since values typed by Any may be futures
        ) {
            return true;
        }

        if (checkedDecls.contains(decl)) {
            return false;
        }
        checkedDecls.add(decl);

        // Check if any type arguments may contain references
        // Expect that they will be used in constructors
        for (Type t : typeArgs) {
            if (t.isReferenceType()) {
                return true;
            } else if (t.isDataType() && ((DataTypeType) t).hasReferences(checkedDecls)) {
                return true;
            }
        }

        // Check if constructors use reference types
        // Type parameters are ignored here
        for (DataConstructor c : decl.getDataConstructors()) {
            for (ConstructorArg arg : c.getConstructorArgs()) {
                Type t = arg.getType();
                if (t.isReferenceType()) {
                    return true;
                } else if (t.isDataType() && ((DataTypeType) t).hasReferences(checkedDecls)) {
                    return true;
                }
            }
        }

        return false;
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
        if (numTypeArgs() != t.numTypeArgs()) return false;
        for (int i = 0; i < numTypeArgs(); i++) {
            if (!getTypeArg(i).equals(t.getTypeArg(i)))
                return false;
        }
        return true;
    }

    @Override
    public boolean isAssignableTo(Type t) {
        return this.isAssignableTo(t, true);
    }

    @Override
    public boolean isAssignableTo(Type t, boolean considerSubtyping) {
        if (super.isAssignableTo(t))
            return true;

        if (!(t instanceof DataTypeType))
            return false;

        DataTypeType dt = (DataTypeType) t;
        if (!dt.decl.equals(this.decl)) {
            // Int can be assigned to Rat
            if (this.isIntType() && dt.isRatType()) return true;
            return false;
        }
        if (numTypeArgs() != dt.numTypeArgs()) return false;
        for (int i = 0; i < numTypeArgs(); i++) {
            if (!getTypeArg(i).isAssignableTo(dt.getTypeArg(i), true))
                return false;

            // Except for future types, type arguments of datatypes may not be
            // upcasted to Any.
            // I.e. this is not ok:
            //   List<Int> li = list[42];
            //   List<Any> la = li; 
            //
            // Because then we would indirectly allow comparisons between
            // different types, e.g.
            //
            //   List<Any> li = list[42];
            //   List<Any> ls = list["Hello"];
            //   println(toString(li == ls));
            if (!isFutureType())
                if (dt.getTypeArg(i).isAnyType() && !this.getTypeArg(i).isAnyType())
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
        for (Annotation a : decl.getAnnotations()) {
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

    public boolean isFloatType() {
        return decl.getName().equals("Float");
    }

    public boolean isStringType() {
        return decl.getName().equals("String");
    }

    public boolean isExceptionType() {
        return decl.getName().equals("Exception");
    }

    public boolean isAnyType() {
        return decl.getName().equals("Any");
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
            List<Type> substitutedArgs = new ArrayList<>();
            for (Type arg : dt.getTypeArgs()) {
                if (!arg.isTypeParameter()) {
                    substitutedArgs.add(arg);
                } else {
                    TypeParameter tp = (TypeParameter) arg;
                    Type st = substitution.get(tp.getDecl().getName());
                    assert st != null : "We're pretty sure getSubstitution() took care of it: " + tp.getDecl().getName();
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
        Map<String, Type> substitution = new HashMap<>();
        ParametricDataTypeDecl pd = (ParametricDataTypeDecl) decl;
        for (int i = 0; i < numTypeArgs(); i++) {
            substitution.put(pd.getTypeParameter(i).getName(), getTypeArg(i));
        }

        return substitution;
    }

    @Override
    public Type applyBinding(Map<TypeParameter, Type> binding) {
        if (hasTypeArgs()) {
            List<Type> argTypes = TypeCheckerHelper.applyBindings(binding, getTypeArgs());
            return new DataTypeType(getDecl(), argTypes);
        } else
            return super.applyBinding(binding);
    }

    @Override
    public DataTypeUse toUse() {
        if (hasTypeArgs()) {
            org.abs_models.frontend.ast.List<TypeUse> ls = new org.abs_models.frontend.ast.List<>();
            for (Type arg : getTypeArgs()) {
                ls.add(arg.toUse());
            }
            return new ParametricDataTypeUse(getQualifiedName(), NullCheckerExtension.getAnnotations(this), ls);
        } else {
            return new DataTypeUse(getQualifiedName(), new org.abs_models.frontend.ast.List<>());
        }
    }
}
