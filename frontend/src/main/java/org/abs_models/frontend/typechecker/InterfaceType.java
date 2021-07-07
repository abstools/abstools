/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.nullable.NullCheckerExtension;
import org.abs_models.frontend.typechecker.nullable.NullableType;

public class InterfaceType extends ReferenceType {
    private final java.util.List<Type> supertypes;
    private final InterfaceDecl decl;

    public InterfaceType(InterfaceDecl decl) {
        this.decl = decl;
        this.supertypes = new java.util.ArrayList<>();
        for (InterfaceTypeUse i : decl.getExtendedInterfaceUses()) {
            supertypes.add(i.getType());
        }
    }

    public InterfaceDecl getDecl() {
        return decl;
    }

    @Override
    public boolean isInterfaceType() {
        return true;
    }

    @Override
    public boolean isDeploymentComponentType() {
        // KLUDGE: we need a proper subtyping check here -- but it's all
        // contained in abslang.abs so we make do for now
        return getQualifiedName().equals("ABS.DC.DeploymentComponent")
            || getQualifiedName().equals("ABS.DC.DC")
            || getQualifiedName().equals("ABS.DC.DeploymentComponentForCloudProvider");
    }


    @Override
    public boolean equals(Object o) {
        if (!(o instanceof InterfaceType))
            return false;
        InterfaceType t = (InterfaceType) o;
        return t.decl.equals(this.decl);
    }

    @Override
    public int hashCode() {
        return decl.hashCode();
    }

    @Override
    public boolean isAssignableTo(Type t, boolean considerSubtyping) {
        if (super.isAssignableTo(t))
            return true;

        if (considerSubtyping) {
            if (isAssignable(t, new HashSet<>()))
                return true;
        }
        return false;
    }

    private boolean isAssignable(Type t, Set<Type> visitedTypes) {
        if (visitedTypes.contains(this))
            return false;

        visitedTypes.add(this);
        if (super.isAssignableTo(t))
            return true;

        for (Type it : supertypes) {
            if (it.isInterfaceType()) { // maybe UnkownType
                if (((InterfaceType)it).isAssignable(t, visitedTypes))
                    return true;
            }
        }
        return false;
    }

    @Override
    public boolean isAssignableTo(Type t) {
        return this.isAssignableTo(t, true);
    }

    @Override
    public String toString() {
        return decl.getName();
    }

    @Override
    public String getModuleName() {
        return decl.getModuleDecl().getName();
    }

    @Override
    public String getSimpleName() {
        return decl.getName();
    }

    @Override
    public MethodSig lookupMethod(String name) {
        return decl.lookupMethod(name);
    }

    @Override
    public Type copy() {
        return new InterfaceType(decl);
    }

    @Override
    public Collection<MethodSig> getAllMethodSigs() {
        return decl.getAllMethodSigs();
    }

    @Override
    public InterfaceTypeUse toUse() {
        return new InterfaceTypeUse(getQualifiedName(), NullCheckerExtension.getAnnotations(this));
    }
}
