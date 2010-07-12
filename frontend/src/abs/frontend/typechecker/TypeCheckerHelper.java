package abs.frontend.typechecker;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Exp;

public class TypeCheckerHelper {
	public static void assertEqualTypes(SemanticErrorList l, ASTNode<?> n, Type expectedType, Type t) {
		if (!expectedType.equals(t))
         l.add(new TypeError(n,ErrorMessage.EXPECTED_TYPE,expectedType,t));
	}

	public static void assertHasType(SemanticErrorList l, Exp e, Type t) {
		if (!e.getType().equals(t))
         l.add(new TypeError(e,ErrorMessage.EXPECTED_TYPE,t,e.getType()));
	}

	public static void checkAssignment(SemanticErrorList l, ASTNode<?> n, Type t, Exp e) {
		 Type te = e.getType();
		 if (!te.isSubtypeOf(t)) {
			 l.add(new TypeError(n,ErrorMessage.CANNOT_ASSIGN,te,t));
		 }
		
	}
}
