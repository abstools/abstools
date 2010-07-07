package abs.common;

import abs.frontend.ast.ASTNode;

public class NotImplementedYetException extends RuntimeException {

    public NotImplementedYetException(ASTNode node) {
        super("The AST element "+node.getClass()+" is not implemented yet.");
    }

    public NotImplementedYetException(String comp, ASTNode node) {
        super("The AST element "+node.getClass()+" is not implemented in the "+comp+", yet");
    }

}
