package abs.frontend.typechecker.locationtypes.infer;

import java.util.Set;

import abs.common.Position;
import abs.frontend.ast.ASTNode;


public class LocationTypeVariable {
    public static final Object VAR_KEY = new Object();
    
    public static int counter = 0;
    
    public static final LocationTypeVariable ALWAYS_NEAR = new LocationTypeVariable();
    public static final LocationTypeVariable ALWAYS_FAR = new LocationTypeVariable();
    public static final LocationTypeVariable ALWAYS_BOTTOM = new LocationTypeVariable();
    
    private int id = ++counter;
    private ASTNode<?> node;
    
    public static LocationTypeVariable newVar(Set<Constraint> constraints, ASTNode<?> n) {
        LocationTypeVariable result = new LocationTypeVariable();
        result.node = n;
        constraints.add(Constraint.declConstraint(result));
        return result;
    }
    
    @Override
    public String toString() {
        String pos = "";
        if (node != null) {
            Position p = new Position(node);
            pos = " at " + p.getPositionString();
        }   
        return "v" + id + pos;
    }
    

}
