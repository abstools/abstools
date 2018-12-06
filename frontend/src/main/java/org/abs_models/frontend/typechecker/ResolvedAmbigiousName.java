/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import java.util.LinkedList;
import java.util.List;

import org.abs_models.frontend.ast.AmbiguousDecl;
import org.abs_models.frontend.ast.Decl;

public class ResolvedAmbigiousName extends ResolvedName {

    private List<ResolvedName> alternatives = new LinkedList<>();

    public ResolvedAmbigiousName(ResolvedName alternative1, ResolvedName alternative2) {
        addAlternative(alternative1);
        addAlternative(alternative2);

    }

    private void addAlternative(ResolvedName alternative) {
        if (alternative instanceof ResolvedAmbigiousName) {
            ResolvedAmbigiousName resolvedAmbigiousName = (ResolvedAmbigiousName) alternative;
            alternatives.addAll(resolvedAmbigiousName.alternatives);
        } else {
            alternatives.add(alternative);
        }
    }

    @Override
    public KindedName getQualifiedName() {
        return getFirstAlternative().getQualifiedName();
    }

    public ResolvedName getFirstAlternative() {
        return alternatives.get(0);
    }

    @Override
    public ResolvedModuleName getModuleName() {
        return getFirstAlternative().getModuleName();
    }

    @Override
    public Decl getDecl() {
        return new AmbiguousDecl("", getAlternatives());
    }

    private List<Decl> getAlternatives() {
        List<Decl> result = new LinkedList<>();
        for (ResolvedName r : alternatives) {
            result.add(r.getDecl());
        }
        return result;
    }


}
