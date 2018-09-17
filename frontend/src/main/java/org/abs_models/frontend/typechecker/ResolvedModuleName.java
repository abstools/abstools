/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import org.abs_models.frontend.ast.ModuleDecl;

public class ResolvedModuleName extends ResolvedName {
    private ModuleDecl decl;

    public ResolvedModuleName(ModuleDecl decl) {
        this.decl = decl;
    }

    @Override
    public KindedName getQualifiedName() {
        return new KindedName(getKind(), decl.getName());
    }

    @Override
    public KindedName.Kind getKind() {
        return KindedName.Kind.MODULE;
    }

    @Override
    public ResolvedModuleName getModuleName() {
        return this;
    }
}
