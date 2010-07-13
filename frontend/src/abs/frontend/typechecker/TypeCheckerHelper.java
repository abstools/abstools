package abs.frontend.typechecker;

import java.util.ArrayList;
import java.util.HashSet;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Exp;
import abs.frontend.ast.List;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PureExp;

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
	
	public static void typeCheckEqualParams(SemanticErrorList l, ASTNode<?> n, 
	        List<ParamDecl> params, List<PureExp> args) 
	{
	    ArrayList<Type> types = getTypes(params);
	    typeCheckEqual(l,n,types,args);
	}
	
    public static void typeCheckEqualDataTypeUses(SemanticErrorList l, ASTNode<?> n, 
            List<DataTypeUse> params, List<PureExp> args) 
    {
        ArrayList<Type> types = getTypesFromDataTypeUse(params);
        typeCheckEqual(l,n,types,args);
    }

    public static void typeCheckEqual(SemanticErrorList l, ASTNode<?> n, 
            ArrayList<Type> params, List<PureExp> args) 
    {
        if (params.size() != args.getNumChild()) {
            l.add(new TypeError(n,ErrorMessage.WRONG_NUMBER_OF_ARGS,params.size(),args.getNumChild()));
        } else {
            for (int i = 0; i < params.size(); i++) {
                Type argType = params.get(i);
                PureExp exp = args.getChild(i);
                exp.typeCheck(l);
                if (!exp.getType().isSubtypeOf(argType)) {
                    l.add(new TypeError(n,ErrorMessage.TYPE_MISMATCH,exp.getType(),argType));
                }
            }
        }
	    
	    
	}

    private static ArrayList<Type> getTypesFromDataTypeUse(List<DataTypeUse> params) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (DataTypeUse u : params) {
            res.add(u.getType());
        }
        return res;
    }

    private static ArrayList<Type> getTypes(List<ParamDecl> params) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (ParamDecl d : params) {
            res.add(d.getType());
        }
        return res;
    }
}
