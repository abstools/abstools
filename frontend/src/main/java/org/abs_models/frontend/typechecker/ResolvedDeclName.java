/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import org.abs_models.frontend.ast.Decl;

/**
 * Can be the name for a function, a data type, a type synonym, a class, an
 * interface, a data constructor
 * 
 * @author jan
 * 
 */
public class ResolvedDeclName extends ResolvedName {

    private Decl decl;
    private ResolvedModuleName moduleName;

    public ResolvedDeclName(ResolvedModuleName moduleName, Decl decl) {
        this.decl = decl;
        this.moduleName = moduleName;
    }

    @Override
    public KindedName getQualifiedName() {
        return new KindedName(getKind(), moduleName.getQualifiedString() + "." + decl.getName());
    }

    @Override
    public ResolvedModuleName getModuleName() {
        return moduleName;
    }

    @Override
    public Decl getDecl() {
        return decl;
    }
}
