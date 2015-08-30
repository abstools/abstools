/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import abs.frontend.ast.ASTNode;

public class NotImplementedYetException extends RuntimeException {

    // note: could be unified with CompilerError.getHelpMessage()
    private static String messagePrefix(ASTNode node) {
        String filename = node.getFileName();
        if (filename == null) filename = "<unknown>";
        return filename + ":" + node.getStartLine() + ":" + node.getStartColumn() + ":";
    }

    public NotImplementedYetException(ASTNode node) {
        super(messagePrefix(node) + "The AST element " + node.getClass() + " is not implemented yet");
    }

    public NotImplementedYetException(ASTNode node, String message) {
        super(messagePrefix(node) + message);
    }

}
