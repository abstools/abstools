package abs.frontend.typechecker.locationtypes.infer;

import java.util.Set;

import abs.common.Position;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.VarDecl;
import abs.frontend.typechecker.locationtypes.LocationType;


public class LocationTypeVariable {
    public static final Object VAR_KEY = new Object();
    
    public static int counter = 0;
    
    public static final LocationTypeVariable ALWAYS_NEAR = new LocationTypeVariable();
    public static final LocationTypeVariable ALWAYS_FAR = new LocationTypeVariable();
    public static final LocationTypeVariable ALWAYS_BOTTOM = new LocationTypeVariable();
    public static final LocationTypeVariable ALWAYS_SOMEWHERE = new LocationTypeVariable();
    
    private LocationType[] parametricFarTypes = new LocationType[0];
    
    private int id = ++counter;
    private ASTNode<?> node;

    private LocationType[] allTypes = LocationType.ALLVISTYPES;
    
    public static LocationTypeVariable newVar(Set<Constraint> constraints, ASTNode<?> n, boolean declared, LocationType[] parametricFarTypes) {
        LocationTypeVariable result = new LocationTypeVariable();
        result.parametricFarTypes = parametricFarTypes;
        result.allTypes = new LocationType[parametricFarTypes.length + LocationType.ALLVISTYPES.length];
        int j = 0;
        for (LocationType lt : LocationType.ALLVISTYPES) {
            result.allTypes[j] = lt;
            j++;
        }
        for (LocationType lt : parametricFarTypes) {
            result.allTypes[j] = lt;
            j++;
        }
        result.node = n;
        constraints.add(Constraint.declConstraint(result));
        if (declared) {
            LocationType[] allCTypes = new LocationType[parametricFarTypes.length + LocationType.ALLCONCRETEUSERTYPES.length];
            int i = 0;
            for (LocationType lt : LocationType.ALLCONCRETEUSERTYPES) {
                allCTypes[i] = lt;
                i++;
            }
            for (LocationType lt : parametricFarTypes) {
                allCTypes[i] = lt;
                i++;
            }
            constraints.add(Constraint.constConstraint(result, allCTypes, Constraint.MUST_HAVE));
        }
        return result;
    }
    
    public LocationType[] parametricFarTypes() {
        return parametricFarTypes;
    }
    
    public LocationType[] allTypes() {
        return allTypes;
    }
    
    @Override
    public String toString() {
        String pos = "";
        if (node != null) {
            Position p = new Position(node);
            pos = " at " + p.getPositionString();
        }  
        return "v" + id + getASTNodeString();
    }

    public ASTNode<?> getTypeNode() {
        if (node instanceof MethodSig) {
            MethodSig ms = (MethodSig) node;
            return ms.getReturnType();
        } 
        
        if (node instanceof FieldDecl) {
            FieldDecl fd = (FieldDecl) node;
            return fd.getAccess();
        }
        
        if (node instanceof ParamDecl) {
            ParamDecl pd = (ParamDecl) node;
            return pd.getAccess();
        }
        
        if (node instanceof VarDecl) {
            VarDecl vd = (VarDecl) node;
            return vd.getAccess();
        }
         
        
        return node;
        
    }
    
    private String getASTNodeString() {
        if (node == null)
            return "";
        
        if (node instanceof MethodSig) {
            MethodSig ms = (MethodSig) node;
            return " Method "+ms.getName();
        } 
        
        if (node instanceof FieldDecl) {
            FieldDecl fd = (FieldDecl) node;
            return " Field "+((ClassDecl)fd.getParent().getParent()).getName()+"."+fd.getName();
        }
        
        if (node instanceof VarDecl) {
            VarDecl vd = (VarDecl) node;
            return " Var "+vd.getName();
        }
         
        
        return node.getClass().getName();
    }
    
    
    public ASTNode<?> getNode() {
        return node;
    }

    public static LocationTypeVariable getFromLocationType(LocationType lt) {
        if (lt.isBottom()) return ALWAYS_BOTTOM;
        if (lt.isFar()) return ALWAYS_FAR;
        if (lt.isNear()) return ALWAYS_NEAR;
        if (lt.isSomewhere()) return ALWAYS_SOMEWHERE;
        throw new IllegalArgumentException("Location type " + lt + " not allowed");
    }
    
    

}
