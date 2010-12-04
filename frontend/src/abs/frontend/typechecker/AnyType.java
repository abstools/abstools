package abs.frontend.typechecker;

public final class AnyType extends Type {
    public static final AnyType INSTANCE = new AnyType();

    @Override
    public Type copy() {
        return INSTANCE;
    }
    
    private AnyType() {
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof AnyType;
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
