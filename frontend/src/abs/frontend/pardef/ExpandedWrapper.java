package abs.frontend.pardef;

import abs.frontend.ast.FunctionDecl;

public final class ExpandedWrapper {

    private final FunctionDecl expanded;
    private final int expansionIndex;

    public ExpandedWrapper(FunctionDecl expanded, int expansionIndex) {
        this.expanded = expanded;
        this.expansionIndex = expansionIndex;
    }

    public FunctionDecl getExpanded() {
        return expanded;
    }

    public int getExpansionIndex() {
        return expansionIndex;
    }
}
