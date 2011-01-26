package abs.frontend.delta.exceptions;

import abs.frontend.ast.ASTNode;

public class ASTNodeNotFoundException extends Exception {

    ASTNode parent;
    String name;

    public ASTNodeNotFoundException(ASTNode p, String n) {
        parent = p;
        name = n;
    }

    public ASTNodeNotFoundException(ASTNode p, ASTNode n) {
        parent = p;
        name = n.toString();
    }

    public String toString() {
        return "ASTNodeNotFoundException[" + name + " in subtree " + parent.toString() + "]";
    }

}
