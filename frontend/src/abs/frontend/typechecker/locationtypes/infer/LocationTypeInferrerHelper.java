package abs.frontend.typechecker.locationtypes.infer;

import java.util.HashMap;
import java.util.Set;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.List;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeCheckerHelper;
import abs.frontend.typechecker.TypeParameter;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.LocationTypeCheckerHelper;
import abs.frontend.typechecker.locationtypes.LocationType.Parametric;

public class LocationTypeInferrerHelper {
    
    
    public static LocationTypeVariable getLocationTypeVar(Type t) {
        return (LocationTypeVariable) t.getMetaData(LocationTypeVariable.VAR_KEY);
    }
        
    public static void checkAssignable(LocationTypeVariable adaptTo, List<ParamDecl> params, List<PureExp> args, Set<Constraint> constraints) {
        java.util.List<Type> paramsTypes = TypeCheckerHelper.getTypes(params);
        for (int i = 0; i < paramsTypes.size(); i++) {
            Type argType = paramsTypes.get(i);
            PureExp exp = args.getChild(i);
            checkAssignable(exp.getType(), argType, adaptTo, constraints);
        }
    }
    
    public static void checkAssignable(Type rht, Type lht, Set<Constraint> constraints) {
        checkAssignable(rht, lht, LocationTypeVariable.ALWAYS_NEAR, constraints);
    }
    
    public static void checkAssignable(Type rht, Type lht, LocationTypeVariable adaptTo, Set<Constraint> constraints) {
        if (lht.isDataType() && rht.isDataType()) {
            DataTypeType dtl = (DataTypeType) lht;
            DataTypeType dtr = (DataTypeType) rht;
            if (dtl.hasTypeArgs() && dtr.hasTypeArgs() && dtl.getTypeArgs().size() == dtr.getTypeArgs().size()) {
                for (int i = 0; i < dtl.getTypeArgs().size(); i++) {
                    checkAssignable(dtr.getTypeArg(i), dtl.getTypeArg(i), adaptTo, constraints);
                }
            }
        }
        if (lht.isReferenceType() && rht.isReferenceType()) {
            LocationTypeVariable sub = getLocationTypeVar(rht);
            LocationTypeVariable tv = getLocationTypeVar(lht);
            if (adaptTo != LocationTypeVariable.ALWAYS_NEAR) { // Optimization
                sub = adaptTo(getLocationTypeVar(rht), adaptTo, constraints, null);
            }
            constraints.add(Constraint.subConstraint(sub, tv));
            //System.out.println("Require " + sub + " < " + tv);
        }
    }

    
    public static LocationTypeVariable adaptTo(LocationTypeVariable expLocType, LocationTypeVariable adaptTo, Set<Constraint> constraints, ASTNode<?> n) {
        LocationTypeVariable tv = LocationTypeVariable.newVar(constraints, n);
        constraints.add(Constraint.adaptConstraint(tv, expLocType, adaptTo));
        //System.out.println("Require " + tv + " = " + expLocType + " |>" + adaptTo);
        return tv;
    }
    
    public static void adaptTo(Type rht, LocationTypeVariable adaptTo, Set<Constraint> constraints) {
        if (rht.isDataType()) {
            DataTypeType dtr = (DataTypeType) rht;
            if (dtr.hasTypeArgs()) {
                for (int i = 0; i < dtr.getTypeArgs().size(); i++) {
                    adaptTo(dtr.getTypeArg(i), adaptTo, constraints);
                }
            }
        }
        if (rht.isReferenceType()) {
            if (adaptTo != LocationTypeVariable.ALWAYS_NEAR) { // Optimization
                LocationTypeVariable tv = adaptTo(getLocationTypeVar(rht), adaptTo, constraints, null);
                annotateVar(rht, tv, constraints);
            }
        }
    }

    public static void annotateVar(Type t, Set<Constraint> constraints, ASTNode<?> n) {
        if (t.isDataType()) {
            DataTypeType dt = (DataTypeType) t;
            if (dt.hasTypeArgs()) {
                for (Type ta : dt.getTypeArgs()) {
                    annotateVar(ta, constraints, n);
                }
            }
        }
        if (t.isReferenceType()) {
            addNewVar(t, constraints, n);
        }
        if (t.isNullType()) {
            annotateVar(t, LocationTypeVariable.ALWAYS_BOTTOM, constraints);
        }
    }
    
    public static void annotateVar(Type t, LocationTypeVariable tv, Set<Constraint> constraints) {
        setIfAnnotated(tv, t, constraints);
        t.addMetaData(LocationTypeVariable.VAR_KEY,tv);
    }
    
    private static void setIfAnnotated(LocationTypeVariable tv, Type t, Set<Constraint> constraints) {
        if (t.isReferenceType()) {
            LocationType lt = LocationTypeCheckerHelper.getLocationType(t, null);
            if (lt != null) {
                constraints.add(Constraint.constConstraint(tv, lt));
            }
        }
    }

    private static LocationTypeVariable addNewVar(Type t, Set<Constraint> constraints, ASTNode<?> n) {
        LocationTypeVariable tv = LocationTypeVariable.newVar(constraints, n);
        annotateVar(t, tv, constraints);
        return tv;
    }
    
    public static void checkEq(LocationTypeVariable tv1, LocationTypeVariable tv2, Set<Constraint> constraints) {
        constraints.add(Constraint.eqConstraint(tv1, tv2));
        //System.out.println("Require " + tv1 + " = " + tv2);
    }
    
    public static void checkDataConstructorExp(DataConstructorExp e, Set<Constraint> constraints) {
        DataConstructor decl = (DataConstructor) e.getDecl();
        if (decl.getDataTypeDecl() instanceof ParametricDataTypeDecl) {
            ParametricDataTypeDecl pd = (ParametricDataTypeDecl) decl.getDataTypeDecl();
            HashMap<TypeParameter, Type> map = new HashMap<TypeParameter, Type>();
            for (int i = 0; i < decl.getNumConstructorArg(); i++) {
                Type t = e.getParam(i).getType();
                Type arg = decl.getConstructorArg(i).getType();
                checkTypeParameter(map, t, arg, constraints);
            }
        }         
    }


    private static void checkTypeParameter(HashMap<TypeParameter, Type> map, Type t, Type arg, Set<Constraint> constraints) {
        if (arg.isTypeParameter()) {
            TypeParameter typeParam = (TypeParameter) arg;
            if (map.containsKey(typeParam)) {
                Type lt = map.get(typeParam);
                // TODO : make this checkEq
                checkAssignable(lt,t,constraints);
                checkAssignable(t,lt,constraints);
            } else {
                map.put(typeParam, t);
            }
        } else if (arg.isDataType()) {
            DataTypeType argdt = (DataTypeType) arg;
            if (argdt.hasTypeArgs()) {
                DataTypeType dt = (DataTypeType)t;
                for (int i = 0; i < dt.numTypeArgs(); i++) {
                     checkTypeParameter(map,dt.getTypeArg(i),argdt.getTypeArg(i), constraints);
                }
            }
        }
    }
}
