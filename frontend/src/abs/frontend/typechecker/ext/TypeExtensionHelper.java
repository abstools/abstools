package abs.frontend.typechecker.ext;

import java.util.ArrayList;
import java.util.HashMap;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Call;
import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.List;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeCheckerHelper;
import abs.frontend.typechecker.TypeParameter;

public class TypeExtensionHelper {
    private final java.util.List<TypeSystemExtension> obs = new ArrayList<TypeSystemExtension>();
    
    public void setSemanticErrorList(SemanticErrorList s) {
        for (TypeSystemExtension tse : obs) {
            tse.setSemanticErrorList(s);
        }
    }
    
    public void register(TypeSystemExtension tse) {
        obs.add(tse);
    }
    
    public void checkMethodCall(Call call) {
        for (TypeSystemExtension tse : obs) {
            tse.checkMethodCall(call);
        }
    }
    
    
    public void checkAssignable(Type callee, List<ParamDecl> params, List<PureExp> args, ASTNode<?> n) {
        java.util.List<Type> paramsTypes = TypeCheckerHelper.getTypes(params);
        for (int i = 0; i < paramsTypes.size(); i++) {
            Type argType = paramsTypes.get(i);
            PureExp exp = args.getChild(i);
            checkAssignable(callee, exp.getType(), argType, n);
        }
    }
    
    public void checkAssignable(Type rht, Type lht, ASTNode<?> n) {
        checkAssignable(null, rht, lht, n);
    }
    
    public void checkAssignable(Type adaptTo, Type rht, Type lht, ASTNode<?> n) {
        if (lht.isDataType() && rht.isDataType()) {
            DataTypeType dtl = (DataTypeType) lht;
            DataTypeType dtr = (DataTypeType) rht;
            if (dtl.hasTypeArgs() && dtr.hasTypeArgs() && dtl.getTypeArgs().size() == dtr.getTypeArgs().size()) {
                for (int i = 0; i < dtl.getTypeArgs().size(); i++) {
                    checkAssignable(adaptTo, dtr.getTypeArg(i), dtl.getTypeArg(i), n);
                }
            }
        }
        if (lht.isReferenceType() && rht.isReferenceType()) {
            for (TypeSystemExtension tse : obs) {
                tse.checkAssignable(adaptTo, rht, lht, n);
            }
        }
    }

    
    public void annotateType(Type t, ASTNode<?> n) {
        if (t.isDataType()) {
            DataTypeType dt = (DataTypeType) t;
            if (dt.hasTypeArgs()) {
                for (Type ta : dt.getTypeArgs()) {
                    annotateType(ta, n);
                }
            }
        } 
        if (t.isReferenceType() || t.isNullType()) {
            for (TypeSystemExtension tse : obs) {
                tse.annotateType(t, n);
            }
        }
    }
    
    public void checkDataConstructorExp(DataConstructorExp e) {
        DataConstructor decl = (DataConstructor) e.getDecl();
        if (decl.getDataTypeDecl() instanceof ParametricDataTypeDecl) {
            HashMap<TypeParameter, Type> map = new HashMap<TypeParameter, Type>();
            for (int i = 0; i < decl.getNumConstructorArg(); i++) {
                Type t = e.getParam(i).getType();
                Type arg = decl.getConstructorArg(i).getType();
                checkTypeParameter(map, t, arg);
            }
        }         
    }


    private void checkTypeParameter(HashMap<TypeParameter, Type> map, Type t, Type arg) {
        if (arg.isTypeParameter() && t.isReferenceType()) {
            TypeParameter typeParam = (TypeParameter) arg;
            if (map.containsKey(typeParam)) {
                Type lt = map.get(typeParam);
                // TODO : make this checkEq
                checkEq(lt,t);
            } else {
                map.put(typeParam, t);
            }
        } else if (arg.isDataType()) {
            DataTypeType argdt = (DataTypeType) arg;
            if (argdt.hasTypeArgs()) {
                DataTypeType dt = (DataTypeType)t;
                for (int i = 0; i < dt.numTypeArgs(); i++) {
                     checkTypeParameter(map,dt.getTypeArg(i),argdt.getTypeArg(i));
                }
            }
        }
    }

    public void checkEq(Type lt, Type t) {
        for (TypeSystemExtension tse : obs) {
            tse.checkEq(lt, t);
        }
    }
    
    public void finished() {
        for (TypeSystemExtension tse : obs) {
            tse.finished();
        }
    }


}
