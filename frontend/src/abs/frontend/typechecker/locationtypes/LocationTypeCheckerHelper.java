package abs.frontend.typechecker.locationtypes;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.List;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeAnnotation;
import abs.frontend.typechecker.TypeCheckerHelper;

public class LocationTypeCheckerHelper {
    
    public static LocationType getLocationType(Type t) {
        LocationType result = null;
        for (TypeAnnotation an : t.getTypeAnnotations()) {
            LocationType lt = getLocationType(an);
            if (lt != null) {
                if (result != null) {
                    throw new LocationTypeCheckerException(new TypeError(an.getValue(),ErrorMessage.LOCATION_TYPE_MULTIPLE, new String[0]));
                }
                result = lt;
            }
        }
        return result;
    }

    private static LocationType getLocationType(TypeAnnotation an) {
        if (an.getType().getQualifiedName().equals("ABS.StdLib.LocationType")) {
            DataConstructorExp de = (DataConstructorExp) an.getValue();
            String name = de.getDecl().getName();
            return LocationType.valueOf(name.toUpperCase());
        }
        return null;
    }
    
    public static void checkAssignable(SemanticErrorList s, ASTNode<?> n, Type rhT, Type lhT) {
        checkAssignable(s, n, rhT, lhT, LocationType.NEAR);
    }
    
    public static void checkAssignable(SemanticErrorList s, ASTNode<?> n, Type rhT, Type lhT, LocationType adaptTo) {
        
        if (rhT.isReferenceType()) {
            LocationType lht =  LocationTypeCheckerHelper.getLocationType(lhT);
            LocationType rht = LocationTypeCheckerHelper.getLocationType(rhT);

            if (!rht.isSubtypeOf(lht)) {
                s.add(new TypeError(n,ErrorMessage.LOCATION_TYPE_CANNOT_ASSIGN,rht.toString(),lht.toString()));
            }
        } 
        
        if (rhT.isDataType()) {
            DataTypeType rhdt = (DataTypeType) rhT;
            DataTypeType lhdt = (DataTypeType) lhT;
            if (rhdt.hasTypeArgs()) {
                for (int i = 0; i < rhdt.numTypeArgs(); i++) {
                    checkAssignable(s,n,rhdt.getTypeArg(i), lhdt.getTypeArg(i), adaptTo);
                }
            }
        }
        

        
    }
    
    public static void checkAssignable(SemanticErrorList l, ASTNode<?> n, LocationType callee, List<ParamDecl> params, List<PureExp> args) {
        java.util.List<Type> paramsTypes = TypeCheckerHelper.getTypes(params);
        for (int i = 0; i < paramsTypes.size(); i++) {
            Type argType = paramsTypes.get(i);
            PureExp exp = args.getChild(i);
            checkAssignable(l,n,exp.getType(),argType,callee);
        }
        
    }
    
}
