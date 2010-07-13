package abs.frontend.typechecker;

import java.util.HashSet;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Exp;
import abs.frontend.ast.List;
import abs.frontend.ast.ParamDecl;

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
	
	public static void typeCheckParamList(SemanticErrorList l, List<ParamDecl> params) {
        HashSet<String> names = new HashSet<String>();
        for (ParamDecl d : params) {
            if (names.contains(d.getName())) {
                l.add(new TypeError(d, ErrorMessage.DUPLICATE_PARAM_NAME, d.getName()));
            } else {
                names.add(d.getName());
            }
            d.typeCheck(l);
        }

	}
}
