package abs.backend.java;

import beaver.Symbol;
import abs.frontend.ast.ASTNode;

public class JavaBackendException extends RuntimeException {
    public JavaBackendException(String msg) {
        super("An exception in the Java backend of ABS occurred: "+msg);
    }

    public JavaBackendException(ASTNode<?> node, String msg) {
        super("An exception in the Java backend of ABS occurred: "+
                Symbol.getLine(node.getStart()) + ":" + Symbol.getColumn(node.getStart()) + ": " + msg);
    }

}
