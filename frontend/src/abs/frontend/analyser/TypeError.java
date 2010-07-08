package abs.frontend.analyser;

import abs.frontend.ast.ASTNode;
import abs.frontend.typechecker.Type;

public class TypeError extends SemanticError {

    public TypeError(ASTNode<?> node, ErrorMessage msg, String... args) {
        super(node, msg, args);
    }

    public TypeError(ASTNode<?> node, ErrorMessage msg, Type... args) {
        super(node, msg, toString(args));
    }

    private static String[] toString(Type[] args) {
        String[] res = new String[args.length];
        for (int i=0; i < args.length; i++) {
            res[i] = args[i].toString();
        }
        return res;
    }
}
