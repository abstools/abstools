/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import abs.frontend.ast.ASTNode;

public class NotImplementedYetException extends RuntimeException {

    public NotImplementedYetException(ASTNode node) {
        super("The AST element " + node.getClass() + " is not implemented yet.");
    }

    public NotImplementedYetException(String comp, ASTNode node) {
        super("The AST element " + node.getClass() + " is not implemented in the " + comp + ", yet");
    }

}
