/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import abs.frontend.ast.Decl;
import abs.frontend.ast.HasTypeHierarchy;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;

public class TypeHierarchyHelper {

    public static Set<InterfaceDecl> addSuperTypes(Set<InterfaceDecl> superTypes, HasTypeHierarchy decl) {
        for (InterfaceDecl direct : decl.getDirectSuperTypes()) {
            boolean isNew = superTypes.add(direct);
            if (isNew) {
                addSuperTypes(superTypes, direct);
            }
        }
        return superTypes;
    }

    public static List<InterfaceDecl> getTypeDecls(Iterable<InterfaceTypeUse> extendedInterfaceUseList) {
        List<InterfaceDecl> result = new ArrayList<>();
        for (InterfaceTypeUse extended : extendedInterfaceUseList) {
            Decl decl = extended.getDecl();
            if (decl instanceof InterfaceDecl) {
                result.add((InterfaceDecl) decl);
            }
        }
        return result;
    }

}
