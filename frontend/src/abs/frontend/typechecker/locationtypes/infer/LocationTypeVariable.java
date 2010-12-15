package abs.frontend.typechecker.locationtypes.infer;

import java.util.Set;

import beaver.Symbol;

import abs.common.Position;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Access;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.VarDecl;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeUse;
import abs.frontend.typechecker.locationtypes.LocationType;


public class LocationTypeVariable {
    public static final Object VAR_KEY = new Object();
    
    public static int counter = 0;
    
    public static final LocationTypeVariable ALWAYS_NEAR = new LocationTypeVariable();
    public static final LocationTypeVariable ALWAYS_FAR = new LocationTypeVariable();
    public static final LocationTypeVariable ALWAYS_BOTTOM = new LocationTypeVariable();
    
    private int id = ++counter;
    private ASTNode<?> node;
    
    public static LocationTypeVariable newVar(Set<Constraint> constraints, ASTNode<?> n, boolean declared) {
        LocationTypeVariable result = new LocationTypeVariable();
        result.node = n;
        constraints.add(Constraint.declConstraint(result));
        if (declared) {
            constraints.add(Constraint.constConstraint(result, LocationType.ALLVISTYPES, Constraint.MUST_HAVE));
        }
        return result;
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
    
    

}
