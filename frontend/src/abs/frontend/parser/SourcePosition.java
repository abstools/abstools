/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.List;
import abs.frontend.ast.Opt;

public class SourcePosition {
    private final int line;
    private final int column;
    private final ASTNode<?> contextNode;

    SourcePosition(ASTNode<?> node, int line, int column) {
        this.line = line;
        this.column = column;
        contextNode = node;
    }

    public ASTNode<?> getContextNode() {
        return contextNode;
    }

    public int getColumn() {
        return column;
    }

    public int getLine() {
        return line;
    }

    public static SourcePosition findPosition(ASTNode<?> node, int searchline, int searchcolumn) {
        if (inNode(node, searchline, searchcolumn)) {
            for (int i = 0; i < node.getNumChildNoTransform(); i++) {
                SourcePosition pos = findPosition(node.getChildNoTransform(i), searchline, searchcolumn);
                if (pos != null) {
                    return pos;
                }
            }
            return new SourcePosition(node, searchline, searchcolumn);
        }

        return null;
    }

    private static boolean inNode(ASTNode<?> node, int line, int column) {
        if (node instanceof Opt<?>) {
            Opt<?> opt = (Opt<?>) node;
            if (opt.hasChildren()) {
                node = opt.getChildNoTransform(0);
            } else {
                return false;
            }
        } else if (node instanceof List<?>) {
            if (node.getNumChildNoTransform() == 0) {
                return false;
            } else if (node.getEndLine() == 0 && node.getEndColumn() == 0) {
                // if position is not set, check children
                if (node.getNumChildNoTransform() == 0) {
                    return false;
                }
                if (smaller(line, column, node.getChildNoTransform(0).getStartLine(), node.getChildNoTransform(0).getStartColumn())
                    || larger(line, column, node.getChildNoTransform(node.getNumChildNoTransform()-1).getEndLine(),
                              node.getChildNoTransform(node.getNumChildNoTransform()-1).getEndColumn())) {
                    return false;
                }
                return true;
            }
        }

        if (smaller(line, column, node.getStartLine(), node.getStartColumn())
            || larger(line, column, node.getEndLine(), node.getEndColumn()))
            return false;
        return true;
    }

    public static boolean larger(int line1, int column1, int line2, int column2) {
        return smaller(line2, column2, line1, column1);
    }

    public static boolean smaller(int line1, int column1, int line2, int column2) {
        if (line1 < line2)
            return true;

        if (line1 > line2)
            return false;

        return column1 < column2;
    }
}
