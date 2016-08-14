/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

import abs.frontend.ast.Decl;

public abstract class ResolvedName {
    public abstract KindedName getQualifiedName();

    public String getQualifiedString() {
        return getQualifiedName().getName();
    }

    public abstract ResolvedModuleName getModuleName();

    public Decl getDecl() {
        return null;
    }

    public KindedName.Kind getKind() {
        if (getDecl().isDataConstructor())
            return KindedName.Kind.DATA_CONSTRUCTOR;
        else if (getDecl().isClass())
            return KindedName.Kind.CLASS;
        else if (getDecl().isFunction())
            return KindedName.Kind.FUN;
        else if(getDecl().isException())
            return KindedName.Kind.EXCEPTION;
        else
            return KindedName.Kind.TYPE_DECL;

    }

    public KindedName getSimpleName() {
        String s = getQualifiedString();
        return new KindedName(getKind(), s.substring(s.lastIndexOf('.') + 1));
    }

    public boolean isModuleName() {
        return false;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ResolvedName))
            return false;
        return ((ResolvedName) obj).getQualifiedString().equals(getQualifiedString());
    }

    @Override
    public int hashCode() {
        return getQualifiedString().hashCode();
    }
}
