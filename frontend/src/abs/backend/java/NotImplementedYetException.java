package abs.backend.java;

import abs.frontend.ast.ASTNode;

public class NotImplementedYetException extends RuntimeException {

    public NotImplementedYetException(ASTNode node) {
        super("The AST element "+node.getClass()+" is not yet implemented in the Java backend");
    }
}
