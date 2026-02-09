package org.abs_models.frontend.typechecker;

import org.abs_models.frontend.ast.*;

import java.util.Collection;
import java.util.HashSet;

/**
 * Helper-datastructure for {@link PureExp#getFields()}.
 */
public class FieldUseInfo {
    final public Collection<FieldUse> fields = new HashSet<FieldUse>();
    public Collection<FunctionDef> seenDefs = new HashSet<FunctionDef>();
    public boolean usesBuiltin = false;

    public FieldUseInfo(){};

    public FieldUseInfo(FieldUse fu) {
        fields.add(fu);
    }

    public FieldUseInfo(FieldUseInfo f) {
        fields.addAll(f.fields);
        usesBuiltin = f.usesBuiltin;
    }

    public FieldUseInfo(Collection<FunctionDef> seen) {
        seenDefs = seen;
    }

    public FieldUseInfo merge(PureExp e) {
        return merge(e.getFields(this));
    }

    public FieldUseInfo merge(Guard g) {
        return merge(g.getFields(this));
    }

    public FieldUseInfo merge(List<PureExp> es) {
        for (PureExp e : es) {
            merge(e);
        }
        return this; // fluent
    }

    public FieldUseInfo merge(FieldUseInfo fui) {
        fields.addAll(fui.fields);
        usesBuiltin |= fui.usesBuiltin;
        return this; // fluent
    }
}
