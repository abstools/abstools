/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Decl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;

public class TypeHierarchyHelper {

    public static List<Decl> getDirectSubTypes(InterfaceDecl interfaceDecl) {
        List<Decl> result = new ArrayList<Decl>();
        CompilationUnit cu = interfaceDecl.getCompilationUnit();
        // iterate over all the types
        // could be done more intelligent, when more performance is needed
        for (ModuleDecl module : cu.getModuleDecls()) {
            for (Decl decl : module.getDeclList()) {
                for (InterfaceDecl superType : getDirectSuperTypes(decl)) {
                    if (superType == interfaceDecl) {
                        result.add(decl);
                    }
                }
            }
        }
        return result;     
    }

    public static List<Decl> getSubTypes(InterfaceDecl interfaceDecl) {
        List<Decl> result = new ArrayList<Decl>();
        Model model = interfaceDecl.getModel();
        // iterate over all the types
        // could be done more intelligent, when more performance is needed
        for (CompilationUnit cu : model.getCompilationUnits()) {
            for (ModuleDecl module : cu.getModuleDecls()) {
                for (Decl decl : module.getDeclList()) {
                    for (InterfaceDecl superType : getSuperTypes(decl)) {
                        if (superType == interfaceDecl) {
                            result.add(decl);
                        }
                    }
                }
            }
        }
        return result;     
    }

    public static List<InterfaceDecl> getDirectSuperTypes(Decl decl) {
        if (decl instanceof InterfaceDecl) {
            InterfaceDecl i = (InterfaceDecl) decl;
            return getTypeDecls(i.getExtendedInterfaceUseList());
        } else if (decl instanceof ClassDecl) {
            ClassDecl c = (ClassDecl) decl;
            return getTypeDecls(c.getImplementedInterfaceUseList());
        } else {
            return Collections.emptyList();
        }
    }

    public static Set<InterfaceDecl> getSuperTypes(Decl decl) {
        Set<InterfaceDecl> superTypes = new HashSet<InterfaceDecl>();
        addSuperTypes(superTypes, decl);
        return superTypes;
    }

    private static void addSuperTypes(Set<InterfaceDecl> superTypes, Decl decl) {
        for (InterfaceDecl direct : getDirectSuperTypes(decl)) {
            boolean isNew = superTypes.add(direct);
            if (isNew) {
                addSuperTypes(superTypes, direct);
            }
        }
    }

    private static List<InterfaceDecl> getTypeDecls(Iterable<InterfaceTypeUse> extendedInterfaceUseList) {
        List<InterfaceDecl> result = new ArrayList<InterfaceDecl>(); 
        for (InterfaceTypeUse extended : extendedInterfaceUseList) {
            Decl decl = extended.getDecl();
            if (decl instanceof InterfaceDecl) {
                result.add((InterfaceDecl) decl);
            }
        }
        return result;
    }

}
