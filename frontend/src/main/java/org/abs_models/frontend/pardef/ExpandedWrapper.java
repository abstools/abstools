package org.abs_models.frontend.pardef;

import org.abs_models.frontend.ast.FunctionDecl;

public final class ExpandedWrapper {

    private final FunctionDecl expanded;
    private final int expansionId;

    public ExpandedWrapper(FunctionDecl expanded, int expansionId) {
        this.expanded = expanded;
        this.expansionId = expansionId;
    }

    public FunctionDecl getExpanded() {
        return expanded;
    }

    public int getExpansionId() {
        return expansionId;
    }
}
