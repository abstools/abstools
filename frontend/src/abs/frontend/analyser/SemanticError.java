package abs.frontend.analyser;

import beaver.Symbol;
import abs.frontend.ast.ASTNode;

public class SemanticError {
    public final ErrorMessage msg;
    public final String[] args;
    public final ASTNode<?> node;
    
    public SemanticError(ASTNode<?> node, ErrorMessage msg, String... args) {
        this.node = node;
        this.msg = msg;
        this.args = args;
    }
    
    public String getMsgString() {
        return Symbol.getLine(node.getStart()) + ":" + 
            Symbol.getColumn(node.getStart()) + ": " + 
            msg.withArgs(args);
    }
}
