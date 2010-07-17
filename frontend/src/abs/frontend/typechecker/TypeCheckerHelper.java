package abs.frontend.typechecker;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Exp;
import abs.frontend.ast.List;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.ParametricFunctionDecl;
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
	    typeCheckEqual(l,n,getTypes(params),args);
	}

	public static void typeCheckMatchingParams(SemanticErrorList l, ASTNode<?> n, 
	        DataConstructor decl, List<PureExp> args) 
	{
		Map<TypeParameter, Type> binding = getTypeParamBindingFromDataTypeUse(decl.getConstructorArgs(), args);
		java.util.List<Type> types = applyBinding(binding, getTypesFromDataTypeUse(decl.getConstructorArgs()));
		typeCheckEqual(l,n,types,args);
	}

	public static void typeCheckMatchingParams(SemanticErrorList l, ASTNode<?> n, 
	        ParametricFunctionDecl decl, List<PureExp> args) 
	{
		Map<TypeParameter, Type> binding = getTypeParamBindingFromParamDecl(decl.getParams(), args);
		java.util.List<Type> types = applyBinding(binding, getTypes(decl.getParams()));
		typeCheckEqual(l,n,types,args);
	}
	
    public static java.util.List<Type> applyBinding(
         Map<TypeParameter, Type> binding, java.util.List<Type> types) 
    {
       ArrayList<Type> res = new ArrayList<Type>();
       for (Type t : types) {
      	 res.add(applyBinding(binding,t));
       }
	   return res;
   }

    public static Type applyBinding(
          Map<TypeParameter, Type> binding, Type t) { 
      	 if (t.isTypeParameter()) {
      		 Type res = binding.get((TypeParameter)t);
      		 if (res == null)
      			 return t;
      		 else
      			 return res;
      	 } else if (t.isDataType()) {
      		 DataTypeType dt = (DataTypeType) t;
      		 if (dt.hasTypeArgs()) {
      			 java.util.List<Type> argTypes = applyBinding(binding,dt.getTypeArgs());
      			 return new DataTypeType(dt.getDecl(),argTypes);
      		 } 
      	 } 
      	 
   		 return t;
	 }

    public static void typeCheckEqualDataTypeUses(SemanticErrorList l, ASTNode<?> n, 
            List<DataTypeUse> params, List<PureExp> args) 
    {
   	 java.util.List<Type> types = getTypesFromDataTypeUse(params);
        typeCheckEqual(l,n,types,args);
    }

    public static void typeCheckEqual(SemanticErrorList l, ASTNode<?> n, 
   		 java.util.List<Type> params, List<PureExp> args) 
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

    public static java.util.List<Type> getTypesFromDataTypeUse(List<DataTypeUse> params) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (DataTypeUse u : params) {
            res.add(u.getType());
        }
        return res;
    }

    private static java.util.List<Type> getTypes(List<ParamDecl> params) {
        ArrayList<Type> res = new ArrayList<Type>();
        for (ParamDecl d : params) {
            res.add(d.getType());
        }
        return res;
    }

    private static java.util.List<Type> getTypesFromExp(List<PureExp> args) {
       ArrayList<Type> res = new ArrayList<Type>();
       for (PureExp e : args) {
           res.add(e.getType());
       }
       return res;
   }
    
    public static Map<TypeParameter, Type> getTypeParamBindingFromDataTypeUse(List<DataTypeUse> params, List<PureExp> args) {
   	 return getTypeParamBinding(getTypesFromDataTypeUse(params),args);
    }
    
   public static Map<TypeParameter, Type> getTypeParamBindingFromParamDecl(List<ParamDecl> params, List<PureExp> args) {
  	 return getTypeParamBinding(getTypes(params),args);
   }
   
   public static Map<TypeParameter, Type> getTypeParamBinding(java.util.List<Type> params, List<PureExp> args) {
   	return getTypeParamBinding(params,getTypesFromExp(args));
   }
   
   public static Map<TypeParameter, Type> getTypeParamBinding(java.util.List<Type> params, java.util.List<Type> args) {
   	Map<TypeParameter, Type> binding = new HashMap<TypeParameter, Type>();
   	 addTypeParamBinding(binding, params, args);
   	 return binding;
   }

   public static void addTypeParamBinding(Map<TypeParameter, Type> binding, java.util.List<Type> params, java.util.List<Type> args) {
   	for (int i = 0; i < params.size(); i++) {
   		Type paramType = params.get(i);
   		Type argType = args.get(i);
   		if (paramType.isTypeParameter()) {
   			binding.put((TypeParameter) paramType, argType);
   		} else if (paramType.isDataType() && argType.isDataType()) {
   			DataTypeType paramdt = (DataTypeType) paramType;
   			DataTypeType argdt = (DataTypeType) argType;
   			if (paramdt.numTypeArgs() == argdt.numTypeArgs()) {
   				addTypeParamBinding(binding, paramdt.getTypeArgs(), argdt.getTypeArgs());
   			}
   		}
   	}
   }
}
