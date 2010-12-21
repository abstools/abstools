package abs.frontend.typechecker.locationtypes.infer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
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
    
    boolean updated = false;
    
    boolean declared = false;
    
    public static final LocationTypeVariable ALWAYS_NEAR = new LocationTypeVariable();
    public static final LocationTypeVariable ALWAYS_FAR = new LocationTypeVariable();
    public static final LocationTypeVariable ALWAYS_BOTTOM = new LocationTypeVariable();
    public static final LocationTypeVariable ALWAYS_SOMEWHERE = new LocationTypeVariable();
    
    private List<LocationType> parametricFarTypes = new ArrayList<LocationType>();
    
    List<LocationType> parametricClassLocalFarTypes = new ArrayList<LocationType>();
    List<LocationType> allCTypes = new ArrayList<LocationType>(Arrays.asList(LocationType.ALLCONCRETEUSERTYPES));
    List<LocationType> parametricMethodLocalFarTypes = new ArrayList<LocationType>();
    
    private int id = ++counter;
    private ASTNode<?> node;

    private List<LocationType> allTypes = Arrays.asList(LocationType.ALLVISTYPES);
    
    public static LocationTypeVariable newVar(Set<Constraint> constraints, ASTNode<?> n, boolean declared, List<LocationType> parametricMethodLocalFarTypes, List<LocationType> parametricClassLocalFarTypes) {
        LocationTypeVariable result = new LocationTypeVariable();
        result.node = n;
        result.parametricMethodLocalFarTypes = parametricMethodLocalFarTypes;
        result.parametricClassLocalFarTypes = parametricClassLocalFarTypes;
        result.declared = declared;
        
        constraints.add(Constraint.declConstraint(result));
        if (declared) {
            constraints.add(Constraint.constConstraint(result, result.allCTypes, Constraint.MUST_HAVE));
        }
        return result;
    }
    
    private void update() {
        if (updated) return;
        parametricFarTypes.addAll(parametricClassLocalFarTypes);
        parametricFarTypes.addAll(parametricMethodLocalFarTypes);
        allTypes = new ArrayList<LocationType>();
        allTypes.addAll(Arrays.asList(LocationType.ALLVISTYPES));
        allTypes.addAll(parametricFarTypes);
        if (declared) {
            allCTypes.addAll(parametricFarTypes);
        }
        updated = true;
    }
    
    public List<LocationType> parametricFarTypes() {
        update();
        return parametricFarTypes;
    }
    
    public List<LocationType> allTypes() {
        update();
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
