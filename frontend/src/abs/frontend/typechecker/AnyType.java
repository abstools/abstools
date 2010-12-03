package abs.frontend.typechecker;

public class AnyType extends Type {
    public static final AnyType INSTANCE = new AnyType();

    private AnyType() {
    }

    @Override
    public boolean isAssignable(Type t) {
        return true;
    }

    public boolean isAnyType() {
        return true;
    }

    @Override
    public String getSimpleName() {
        return "Any";
    }
    
}
