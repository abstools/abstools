package abs.frontend.typechecker.locationtypes;

import java.util.ArrayList;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.List;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.ReferenceType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeAnnotation;
import abs.frontend.typechecker.TypeCheckerHelper;

public class LocationTypeCheckerHelper {
    
    public static java.util.List<LocationType> getLocationTypes(java.util.List<Type> types, LocationType defaultType) {
        ArrayList<LocationType> res = new ArrayList<LocationType>();
        for (Type t : types) {
            res.add(getLocationType(t,defaultType));
        }
        return res;
    }
    
    public static LocationType getLocationType(Type t, LocationType defaultType) {
        if (t.isNullType())
            return LocationType.BOTTOM;
        
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
            checkAssignable(l,n,exp.getLocationType().adaptTo(adaptTo),getLocationType(argType,defaultType));
        }
        
    }
    
}
