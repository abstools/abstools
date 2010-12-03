package abs.frontend.typechecker.locationtypes;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Name;

public class LocationTypeError extends SemanticError {

    public LocationTypeError(ASTNode<?> node, ErrorMessage msg, Name[] args) {
        super(node, msg, args);
    }

}
