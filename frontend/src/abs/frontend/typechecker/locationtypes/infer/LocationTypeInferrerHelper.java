package abs.frontend.typechecker.locationtypes.infer;

import java.util.Set;

import abs.frontend.ast.List;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeCheckerHelper;

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
        t.addMetaData(LocationTypeVariable.VAR_KEY,tv);
    }
    
    private static LocationTypeVariable addNewVar(Type t, Set<Constraint> constraints, ASTNode<?> n) {
        LocationTypeVariable v = LocationTypeVariable.newVar(constraints, n);
        t.addMetaData(LocationTypeVariable.VAR_KEY,v);
        return v;
    }
    
    public static void checkEq(LocationTypeVariable tv1, LocationTypeVariable tv2, Set<Constraint> constraints) {
        constraints.add(Constraint.eqConstraint(tv1, tv2));
        //System.out.println("Require " + tv1 + " = " + tv2);
    }
}
