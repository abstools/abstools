/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker;

public class KindedName {
    public enum Kind {
        FUN, CLASS, TYPE_DECL, EXCEPTION, DATA_CONSTRUCTOR, MODULE;
    }

    private final String name;
    private final Kind kind;

    public KindedName(Kind kind, String name) {
        this.kind = kind;
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public Kind getKind() {
        return kind;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof KindedName))
            return false;
        KindedName o = (KindedName) obj;
        return name.equals(o.name) && kind.equals(o.kind);
    }

    @Override
    public int hashCode() {
        return name.hashCode() * kind.hashCode();
    }

    @Override
    public String toString() {
        return name + "[" + kind.name() + "]";
    }

    public String getModuleName() {
        String qualifiedName = getName();
        return qualifiedName.substring(0, qualifiedName.lastIndexOf('.'));
    }

    public String getSimpleName() {
        String name = getName();
        final int pos = name.lastIndexOf('.');
        return (pos == -1) ? name : name.substring(pos + 1, name.length());
    }
    
    public boolean isQualified() {
        return name.indexOf('.') > -1;
    }
}
