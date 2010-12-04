package abs.frontend.typechecker;

public final class UnknownType extends Type {
    public static final UnknownType INSTANCE = new UnknownType();

    private UnknownType() {
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof UnknownType;
    }
    
    @Override
    public boolean isUnknownType() {
        return true;
    }

    @Override
    public boolean isAssignable(Type t) {
        return false;
    }

    public String toString() {
        return getSimpleName();
    }

    /**
     * Returns "<UNKOWN>"
     */
    @Override
    public String getSimpleName() {
        return "<UNKNOWN>";
    }

    @Override
    public Type copy() {
        return new UnknownType();
    }
}
