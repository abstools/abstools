package abs.frontend.pardef;

import abs.frontend.ast.ASTNode;

public class PardefModellingException extends RuntimeException {

    public PardefModellingException() {
        super();
    }

    public PardefModellingException(String message) {
        super(message);
    }

    public PardefModellingException(String message, Throwable cause) {
        super(message, cause);
    }

    public PardefModellingException(ASTNode<?> node, String message) {
        // TODO add info about node
        super(message);
    }

    public PardefModellingException(ASTNode<?> node, String message, Throwable cause) {
        // TODO add info about node
        super(message, cause);
    }
}
