package abs.frontend.typechecker;

public final class NullType extends ReferenceType {
    public static NullType INSTANCE = new NullType();

    private NullType() {
        
    }

    @Override
    public boolean isNullType() {
        return true;
    }
    
    @Override
   public boolean isSubtypeOf(Type t) {
      return t instanceof ReferenceType;
   }
    
    @Override
    public String toString() {
        return "NullType";
    }
}
