/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.List;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;

public class UnionType extends ReferenceType {
    private final java.util.List<InterfaceType> types;
    private final ClassDecl originatingClass;

    public UnionType(ClassDecl classDecl, List<InterfaceTypeUse> types) {
        this.types = new ArrayList<>();
        for (InterfaceTypeUse t : types) {
            if (t.getType() instanceof InterfaceType) // maybe UnkownType
                this.types.add((InterfaceType) t.getType());
        }
        originatingClass = classDecl;
    }

    public UnionType(ClassDecl classDecl, InterfaceType... types) {
        this.types = new ArrayList<>();
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
    public boolean isAssignableTo(Type t) {
        if (super.isAssignableTo(t))
            return true;

        for (InterfaceType it : types) {
            if (it.isAssignableTo(t))
                return true;
        }
        return false;
    }

    @Override
    public MethodSig lookupMethod(String name) {
        // Check declared type first...
        MethodImpl m = originatingClass.lookupMethod(name);
        if (m != null)
          return m.getMethodSig();
        // ... then interfaces.
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

    @Override
    public boolean isDeploymentComponentType() {
        for (InterfaceType i : getTypes()) {
            if (i.isDeploymentComponentType()) return true;
        }
        return false;
    }

    public ClassDecl getOriginatingClass() {
        return originatingClass;
    }


    // Use the name of the originating class
    @Override
    public String getModuleName() {
        return originatingClass.getModuleDecl().getName();
    }

    @Override
    public String getSimpleName() {
        return originatingClass.getName();
    }

    @Override
    public Type copy() {
        return new UnionType(originatingClass,types.toArray(new InterfaceType[0]));
    }

    @Override
    public Collection<MethodSig> getAllMethodSigs() {
        return originatingClass.getAllMethodSigs();
    }

    @Override
    public Collection<FieldDecl> getAllFieldDecls() {
        ArrayList<FieldDecl> res = new ArrayList<>();
        for (FieldDecl d : originatingClass.getFieldList()) {
            res.add(d);
        }
        return res;
    }
}
