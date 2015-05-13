/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.List;
import abs.frontend.ast.Opt;
import beaver.Symbol;

public class SourcePosition {
    private final int pos;
    private final ASTNode<?> contextNode;

    SourcePosition(ASTNode<?> node, int pos) {
        this.pos = pos;
        contextNode = node;
    }

    public ASTNode<?> getContextNode() {
        return contextNode;
    }

    public int getColumn() {
        return Symbol.getColumn(pos);
    }

    public int getLine() {
        return Symbol.getLine(pos);
    }

    public static SourcePosition findPosition(ASTNode<?> node, int line, int col) {
        return findPosition(node, Symbol.makePosition(line, col));
    }

    /**
     * [TODO: Review] Here we are also called from the Eclipse scanner, so don't apply rewrites?
     */
    private static SourcePosition findPosition(ASTNode<?> node, int searchPos) {
        if (inNode(node, searchPos)) {
            for (int i = 0; i < node.getNumChildNoTransform(); i++) {
                SourcePosition pos = findPosition(node.getChildNoTransform(i), searchPos);
                if (pos != null) {
                    return pos;
                }
            }
            return new SourcePosition(node, searchPos);
        }

        return null;
    }

    private static boolean inNode(ASTNode<?> node, int pos) {
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
            } else if (node.getEnd() == 0) {
                // if position is not set, check children
                if (node.getNumChildNoTransform() == 0) {
                    return false;
                }
                if (smaller(pos, node.getChildNoTransform(0).getStart()) || larger(pos, node.getChildNoTransform(node.getNumChildNoTransform()-1).getEnd())) {
                    return false;
                }
                return true;
            }
        }
//        int col = Symbol.getColumn(node.getStart());
//        int line = Symbol.getLine(node.getStart());
//        int endcol = Symbol.getColumn(node.getEnd());
//        int endline = Symbol.getLine(node.getEnd());
//        int posCol = Symbol.getColumn(pos);
//        int posLine = Symbol.getLine(pos);
//        
//        System.out.println("in node " + node.getParent().hashCode() + " > " + node.hashCode() + " " + node.getClass() + " " + node);
//        System.out.println("    start: " + line + ":" + col);
//        System.out.println("    pos:   " + posLine + ":" + posCol);
//        System.out.println("    end:   " + endline + ":" + endcol);
        
        if (smaller(pos, node.getStart()) || larger(pos, node.getEnd()))
            return false;
        return true;
    }

    private static boolean larger(int pos, int pos2) {
        return smaller(pos2, pos);
    }

    private static boolean smaller(int pos, int pos2) {
        int col = Symbol.getColumn(pos);
        int line = Symbol.getLine(pos);
        int col2 = Symbol.getColumn(pos2);
        int line2 = Symbol.getLine(pos2);

        if (line < line2)
            return true;

        if (line > line2)
            return false;

        return col < col2;
    }
}
