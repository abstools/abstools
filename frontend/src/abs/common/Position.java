package abs.common;

import beaver.Symbol;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.CompilationUnit;

public class Position {
    private final int col;
    private final int line;
    private final String fileName;

    public Position(ASTNode<?> node) {
        line = Symbol.getLine(node.getStart());
        col = Symbol.getColumn(node.getStart());
        fileName = calcFileName(node);
    }

    private String calcFileName(ASTNode<?> node) {
        ASTNode<?> parent = node;
        while (!(parent instanceof CompilationUnit)) {
            parent = parent.getParent();
            if (parent == null)
                return "<could not find filename>";
        }
        CompilationUnit u = (CompilationUnit) parent;
        String name = u.getName();
        if (name == null)
            return "<unkown>";
        return name;
    }

    public String getPositionString() {
        return getFileName() + ":" + getLine() + ":" + getColumn() + ": ";
    }

    public int getLine() {
        return line;
    }

    public int getColumn() {
        return col;
    }

    private String getFileName() {
        return cutOffPath(fileName);
    }

    private static String cutOffPath(String name) {
        if (name.contains("/")) {
            return name.substring(name.lastIndexOf('/') + 1);
        }
        if (name.contains("\\")) {
            return name.substring(name.lastIndexOf('\\') + 1);
        }
        return name;
    }

}
