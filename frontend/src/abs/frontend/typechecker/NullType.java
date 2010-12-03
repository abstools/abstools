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
    public boolean equals(Object o) {
        return o == INSTANCE;
    }
    
    @Override
    public boolean isAssignable(Type t) {
        if (super.isAssignable(t))
            return true;
        return t instanceof ReferenceType;
    }

    @Override
    public String toString() {
        return "NullType";
    }

    @Override
    public String getModuleName() {
        return null;
    }

    @Override
    public String getSimpleName() {
        return null;
    }
}
