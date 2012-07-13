/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

import abs.frontend.ast.ModuleDecl;
import abs.frontend.typechecker.KindedName.Kind;

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
    public Kind getKind() {
        return Kind.MODULE;
    }

    @Override
    public ResolvedModuleName getModuleName() {
        return this;
    }
}
