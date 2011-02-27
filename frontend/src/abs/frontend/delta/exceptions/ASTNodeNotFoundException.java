package abs.frontend.delta.exceptions;

import abs.frontend.ast.ASTNode;

public class ASTNodeNotFoundException extends Exception {
    public ASTNodeNotFoundException(String msg) {
        super(msg);
    }
}
