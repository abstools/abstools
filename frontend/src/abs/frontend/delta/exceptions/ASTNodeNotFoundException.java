package abs.frontend.delta.exceptions;

import abs.frontend.ast.ASTNode;

public class ASTNodeNotFoundException extends Exception {
    String message;

    public ASTNodeNotFoundException(String msg) {
        message = msg;
    }


    public String toString() {
        return "AST Node Not Found: " + message;
    }

}
