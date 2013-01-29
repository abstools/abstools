/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import beaver.Symbol;
import abs.frontend.ast.ASTNode;

public class Position {
    private final int col;
    private final int line;
    private final String fileName;

    public Position(ASTNode<?> node) {
        line = Symbol.getLine(node.getStart());
        col = Symbol.getColumn(node.getStart());
        fileName = calcFileName(node);
        assert fileName != null;
    }

    private String calcFileName(ASTNode<?> node) {
        String res = node.getFileName();
        return "".equals(res) ? "<unkown>" : res;
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
