package abs.frontend.typechecker;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;

public class TypeCheckerHelper {
	public static void assertEqualTypes(SemanticErrorList l, ASTNode n, Type expectedType, Type t) {
		if (!expectedType.equals(t))
         l.add(new TypeError(n,ErrorMessage.EXPECTED_TYPE,expectedType,t));
	}
}
