package abs.frontend.typechecker.locationtypes;

import java.util.ArrayList;
import java.util.HashMap;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.List;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.ReferenceType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeAnnotation;
import abs.frontend.typechecker.TypeCheckerHelper;
import abs.frontend.typechecker.TypeParameter;
import abs.frontend.typechecker.locationtypes.LocationType.Parametric;

public class LocationTypeCheckerHelper {
    
    public static java.util.List<LocationType> getLocationTypes(java.util.List<Type> types, LocationType defaultType) {
        ArrayList<LocationType> res = new ArrayList<LocationType>();
        for (Type t : types) {
            res.add(getLocationType(t,defaultType));
        }
        return res;
    }
    
    
    public static void checkDataConstructorExp(SemanticErrorList l, DataConstructorExp e) {
        DataConstructor decl = (DataConstructor) e.getDecl();
        if (decl.getDataTypeDecl() instanceof ParametricDataTypeDecl) {
            ParametricDataTypeDecl pd = (ParametricDataTypeDecl) decl.getDataTypeDecl();
            HashMap<TypeParameter, LocationType> map = new HashMap<TypeParameter, LocationType>();
            for (int i = 0; i < decl.getNumConstructorArg(); i++) {
                LocationType t = null; //e.getParam(i).getLocationType();
                Type arg = decl.getConstructorArg(i).getType();
                checkTypeParameter(l, e, map, t, arg);
            }
        }         
    }


    private static void checkTypeParameter(SemanticErrorList l, DataConstructorExp e,
            HashMap<TypeParameter, LocationType> map, LocationType t, Type arg) {
        if (arg.isTypeParameter()) {
            TypeParameter typeParam = (TypeParameter) arg;
            if (map.containsKey(typeParam)) {
                LocationType lt = map.get(typeParam);
                if (! (lt.isSubtypeOf(t) && t.isSubtypeOf(lt))) {
                    l.add(new TypeError(e,ErrorMessage.LOCATION_TYPE_DIFFERENT_TYPE_INSTANTIATIONS,typeParam.getSimpleName(),e.getDecl().getName(),lt.toString(),t.toString()));
                }
            } else {
                map.put(typeParam, t);
            }
        } else if (arg.isDataType()) {
            DataTypeType dt = (DataTypeType) arg;
            if (t.isParametric()) {
                LocationType.Parametric lparam = (Parametric) t;
                for (int i = 0; i < dt.numTypeArgs(); i++) {
                     checkTypeParameter(l,e,map,lparam.typeParams.get(i),dt.getTypeArg(i));
                }
            }
        }
    }
    
    public static LocationType getLocationType(Type t, LocationType defaultType) {
        if (t.isNullType())
            return LocationType.BOTTOM;
        
        LocationType meta = (LocationType) t.getMetaData(LocationType.LOCATION_KEY);
        if (meta != null) {
            return meta;
        }
        
        LocationType result = LocationType.NOTYPE;
        if (t.isDataType()) {
            DataTypeType dt = (DataTypeType) t;
            if (dt.hasTypeArgs()) {
                return new LocationType.Parametric(getLocationTypes(dt.getTypeArgs(), defaultType));
            }
        } else if (t.isReferenceType()){
            boolean found = false;
            for (TypeAnnotation an : t.getTypeAnnotations()) {
                LocationType lt = getLocationType(an);
                if (lt != null) {
                    if (found) {
                        throw new LocationTypeCheckerException(new TypeError(an.getValue(),ErrorMessage.LOCATION_TYPE_MULTIPLE, new String[0]));
                    }
                    found = true;
                    result = lt;
                }
            }
            
            if (found == false) {
                result = defaultType;
            }
        } else if (t.isBoundedType()) {
            result = LocationType.UNBOUND;
        }
        return result;
    }

    private static LocationType getLocationType(TypeAnnotation an) {
        if (an.getType().getQualifiedName().equals("ABS.StdLib.LocationType")) {
            DataConstructorExp de = (DataConstructorExp) an.getValue();
            String name = de.getDecl().getName();
            return LocationType.createFromName(name);
        }
        return null;
    }
    
    public static void checkAssignable(SemanticErrorList s, ASTNode<?> n, PureExp rhtExp, PureExp lhtExp, LocationType defaultType) {
        
        LocationType lht = LocationTypeCheckerHelper.getLocationType(lhtExp.getType(),defaultType);
        LocationType rht = LocationTypeCheckerHelper.getLocationType(rhtExp.getType(),defaultType);
        checkAssignable(s,n,rht,lht);
    }
        
    public static void checkAssignable(SemanticErrorList s, ASTNode<?> n, LocationType rht, LocationType lht) {
        if (!rht.isSubtypeOf(lht)) {
            s.add(new TypeError(n,ErrorMessage.LOCATION_TYPE_CANNOT_ASSIGN,rht.toString(),lht.toString()));
        }
    }
    
    public static void checkAssignable(SemanticErrorList l, ASTNode<?> n, LocationType adaptTo, List<ParamDecl> params, List<PureExp> args, LocationType defaultType) {
        java.util.List<Type> paramsTypes = TypeCheckerHelper.getTypes(params);
        for (int i = 0; i < paramsTypes.size(); i++) {
            Type argType = paramsTypes.get(i);
            PureExp exp = args.getChild(i);
            LocationType expLocType = null; //exp.getLocationType();
            LocationType adaptTo2 = expLocType.adaptTo(adaptTo);
            LocationType argLocType = getLocationType(argType,defaultType);
            checkAssignable(l,n,adaptTo2,argLocType);
        }
        
    }
    
    public static LocationType getLocationType(Type type) {
        return (LocationType) type.getMetaData(LocationType.LOCATION_KEY);
    }

/*    
    public static Type withDefaultType(Type type, LocationType defaultType) {
        if (hasLocationType(type))
            return type;
        return withLocationType(type,defaultType);
    }
*/

    private static boolean hasLocationType(Type type) {
        return null != type.getMetaData(LocationType.LOCATION_KEY);
    }
    
    public static Type withCalcLocationType(Type type, LocationType defaultType) {
        return withLocationType(type, getLocationType(type, defaultType));
    }
    
    public static Type withLocationType(Type type, LocationType lt) {
        Type copy = type.fullCopy();
        copy.addMetaData(LocationType.LOCATION_KEY, lt);
        return copy;
    }
    
    public static Type adaptTo(Type type, Type to) {
        return withLocationType(type,getLocationType(type).adaptTo(getLocationType(to)));
    }
    
}
