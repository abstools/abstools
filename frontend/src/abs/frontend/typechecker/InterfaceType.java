package abs.frontend.typechecker;

import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodSig;

public class InterfaceType extends ReferenceType {
    private final InterfaceDecl decl;
    
    public InterfaceType(InterfaceDecl decl) {
        this.decl = decl;
    }
    
    public InterfaceDecl getDecl() {
        return decl;
    }
    
    @Override
    public boolean isInterfaceType() {
        return true;
    }
    
    @Override
    public boolean equals(Object o) {
        if (!super.equals(o))
            return false;
        InterfaceType t = (InterfaceType) o;
        return t.decl.equals(this.decl);
    }
    
    @Override
    public int hashCode() {
        return decl.hashCode();
    }
    
    @Override
    public boolean isSubtypeOf(Type t) {
   	 if (this.equals(t))
   		 return true;
   	 
 	   if (t.isBoundedType()) {
    		 BoundedType bt = (BoundedType) t;
    		 if (bt.hasBoundType())
    			 return this.isSubtypeOf(bt.getBoundType());
    		 bt.bindTo(this);
    		 return true;
    	 }

 		 for (InterfaceTypeUse i : decl.getExtendedInterfaceUses()) {
 			 if (i.getType().isSubtypeOf(t))
 				 return true;
   	 }
   	 
   	 return false;
    }
    
    @Override
   public String toString() {
      return decl.getName();
   }
    
    @Override
    public MethodSig lookupMethod(String name) {
        return decl.lookupMethod(name);
    }
    
}
