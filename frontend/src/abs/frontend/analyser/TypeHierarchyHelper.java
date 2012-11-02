/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Decl;
import abs.frontend.ast.HasTypeHierarchy;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;

public class TypeHierarchyHelper {

    public static List<Decl> getDirectSubTypes(InterfaceDecl interfaceDecl) {
        List<Decl> result = new ArrayList<Decl>();
        Model model = interfaceDecl.getModel();
        // iterate over all the types
        // could be done more intelligent, when more performance is needed
        for (Decl decl : model.getDecls()) {
            if (decl.getDirectSuperTypes().contains(interfaceDecl))
                result.add(decl);
        }
        return result;     
    }

    public static List<Decl> getSubTypes(InterfaceDecl interfaceDecl) {
        List<Decl> result = new ArrayList<Decl>();
        Model model = interfaceDecl.getModel();
        // iterate over all the types
        // could be done more intelligent, when more performance is needed
        for (Decl decl : model.getDecls()) {
            if (getSuperTypes(decl).contains(interfaceDecl))
                result.add(decl);
        }
        return result;     
    }

    private static Set<InterfaceDecl> getSuperTypes(Decl decl) {
        return addSuperTypes(new HashSet<InterfaceDecl>(), decl);
    }

    public static Set<InterfaceDecl> addSuperTypes(Set<InterfaceDecl> superTypes, Decl decl) {
        if (decl instanceof HasTypeHierarchy) {
            for (InterfaceDecl direct : decl.getDirectSuperTypes()) {
                boolean isNew = superTypes.add(direct);
                if (isNew) {
                    addSuperTypes(superTypes, direct);
                }
            }
        }
        return superTypes;
    }

    public static List<InterfaceDecl> getTypeDecls(Iterable<InterfaceTypeUse> extendedInterfaceUseList) {
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
